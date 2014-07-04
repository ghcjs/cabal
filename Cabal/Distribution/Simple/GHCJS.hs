module Distribution.Simple.GHCJS (
        configure, getInstalledPackages,
        getPackageDBContents,
        buildLib, buildExe,
        installLib, installExe,
        libAbiHash,
        initPackageDB,
        registerPackage,
        componentGhcOptions,
        ghcjsLibDir,
        ghcjsDynamic,
        invokeHcPkg
  ) where

import Control.Monad (when)
import Distribution.PackageDescription as PD
         ( PackageDescription, BuildInfo(..), Executable(..)
         , Library(..), libModules )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.Simple.Compiler
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(..), ComponentLocalBuildInfo(..) )
import Distribution.Simple.InstallDirs hiding ( absoluteInstallDirs )
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Simple.Program
         ( Program(..), ConfiguredProgram(..), ProgramConfiguration
         , ProgramSearchPath, rawSystemProgramConf
         , rawSystemProgramStdoutConf, rawSystemProgramStdout
         , requireProgramVersion, getProgramInvocationOutput
         , userMaybeSpecifyPath, programPath
         , addKnownPrograms, updateProgram, lookupProgram
         , ghcjsProgram, ghcjsPkgProgram, hsc2hsProgram
         , c2hsProgram, stripProgram, requireProgram
         , ldProgram
         )
import Distribution.PackageDescription as PD
         ( PackageDescription(..), BuildInfo(..), Executable(..)
         , Library(..), libModules, exeModules, hcOptions
         , usedExtensions, allExtensions )
import qualified Distribution.Simple.Program.HcPkg as HcPkg
-- import Distribution.Simple.Program.GHC ( GhcOptions )
import Distribution.Simple.Program.GHC
import qualified Distribution.Simple.Setup as Cabal
        ( Flag )
import Distribution.Text ( display )
import Language.Haskell.Extension ( Language(..), Extension(..) )
import Distribution.Verbosity
import Distribution.Version
         ( Version(..), anyVersion, orLaterVersion )
import Distribution.Simple.Program.Builtin ( haddockProgram )
import Distribution.Simple.BuildPaths
import Distribution.Simple.Utils
import Distribution.System ( Platform )
import System.Directory ( doesFileExist )
import System.FilePath  ( (</>), (<.>), takeDirectory, splitExtension )
import qualified Data.Map as M
import System.IO.Error ( catchIOError )
import qualified Control.Exception as E
import qualified Distribution.Simple.GHC.Base as Base
import qualified Distribution.Simple.GHC as GHC
import Distribution.Simple.Setup
         ( toFlag, fromFlag, fromFlagOrDefault )
import Data.Monoid
import Distribution.Package
import Data.Maybe
import Data.List (nub)
import Control.Monad (unless)
import Language.Haskell.Extension
import System.FilePath          ( (</>), (<.>), takeExtension,
                                  takeDirectory, replaceExtension,
                                  splitExtension )
import qualified Distribution.ModuleName as ModuleName
import qualified Distribution.Simple.Program.Ar    as Ar
import qualified Distribution.Simple.Program.Ld    as Ld
import qualified Distribution.Simple.Program.Strip as Strip
import System.Directory
         ( getDirectoryContents, doesFileExist, getTemporaryDirectory )
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(..), ComponentLocalBuildInfo(..)
         , LibraryName(..), absoluteInstallDirs )
import Distribution.System
         ( Platform(..), OS(..), buildOS, platformFromTriple )
import Data.Char (isSpace)
import Distribution.Simple.PackageIndex (PackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
                                ( InstalledPackageInfo_(..) )

configure :: Verbosity -> Maybe FilePath -> Maybe FilePath
          -> ProgramConfiguration
          -> IO (Compiler, Maybe Platform, ProgramConfiguration)
configure verbosity hcPath hcPkgPath conf0 = do
  (ghcjsProg, ghcjsVersion, conf1) <-
    requireProgramVersion verbosity ghcjsProgram
      (orLaterVersion (Version [0,1] []))
      (userMaybeSpecifyPath "ghcjs" hcPath conf0)

  Just ghcjsGhcVersion <- findGhcjsGhcVersion verbosity (programPath ghcjsProg)

  -- This is slightly tricky, we have to configure ghcjs first, then we use the
  -- location of ghcjs to help find ghcjs-pkg in the case that the user did not
  -- specify the location of ghc-pkg directly:
  (ghcjsPkgProg, ghcjsPkgVersion, conf2) <-
    requireProgramVersion verbosity ghcjsPkgProgram {
      programFindLocation = guessGhcjsPkgFromGhcjsPath ghcjsProg
    }
    anyVersion (userMaybeSpecifyPath "ghcjs-pkg" hcPkgPath conf1)

  Just ghcjsPkgGhcVersion <- findGhcjsPkgGhcVersion
                               verbosity (programPath ghcjsPkgProg)

  when (ghcjsVersion /= ghcjsPkgVersion) $ die $
       "Version mismatch between ghcjs and ghcjs-pkg: "
    ++ programPath ghcjsProg ++ " is version " ++ display ghcjsVersion ++ " "
    ++ programPath ghcjsPkgProg ++ " is version " ++ display ghcjsPkgVersion

  when (ghcjsGhcVersion /= ghcjsPkgGhcVersion) $ die $
       "Version mismatch between ghcjs and ghcjs-pkg: "
    ++ programPath ghcjsProg 
    ++ " was built with GHC version " ++ display ghcjsGhcVersion ++ " "
    ++ programPath ghcjsPkgProg
    ++ " was built with GHC version " ++ display ghcjsPkgGhcVersion

  -- be sure to use our versions of hsc2hs, c2hs, haddock and ghc
  let hsc2hsProgram' =
        hsc2hsProgram { programFindLocation =
                          guessHsc2hsFromGhcjsPath ghcjsProg }
      c2hsProgram' =
        c2hsProgram { programFindLocation =
                          guessC2hsFromGhcjsPath ghcjsProg }

      haddockProgram' =
        haddockProgram { programFindLocation =
                          guessHaddockFromGhcjsPath ghcjsProg }
      conf3 = addKnownPrograms [ hsc2hsProgram', c2hsProgram', haddockProgram' ] conf2

  languages  <- getLanguages  verbosity ghcjsProg
  extensions <- getExtensions verbosity ghcjsProg
  let ghcjsProg' = ghcjsProg { programVersion = Just ghcjsGhcVersion }

  ghcInfo <- GHC.getGhcInfo verbosity ghcjsProg'
  let ghcInfoMap = M.fromList ghcInfo

  let comp = Compiler {
        compilerId         = CompilerId GHCJS ghcjsVersion
          (Just (CompilerId GHC ghcjsGhcVersion Nothing)),
        compilerLanguages  = languages,
        compilerExtensions = extensions,
        compilerProperties = ghcInfoMap
      }
      compPlatform = Base.targetPlatform ghcInfo
  -- configure gcc and ld
  let conf4      = if ghcjsNativeToo comp
                     then Base.configureToolchain ghcjsProg' ghcInfoMap conf3
                     else conf3
  return (comp, compPlatform, conf4)

ghcjsNativeToo :: Compiler -> Bool
ghcjsNativeToo =  ghcjsLookupProperty "Native Too"

getLanguages :: Verbosity -> ConfiguredProgram -> IO [(Language, Flag)]
getLanguages verbosity ghcjsProg =
  Base.getLanguages verbosity =<< withGhcVersion verbosity ghcjsProg

getExtensions :: Verbosity -> ConfiguredProgram -> IO [(Extension, Flag)]
getExtensions verbosity ghcjsProg = do
  Base.getExtensions verbosity =<< withGhcVersion verbosity ghcjsProg

guessGhcjsPkgFromGhcjsPath :: ConfiguredProgram -> Verbosity
                           -> ProgramSearchPath -> IO (Maybe FilePath)
guessGhcjsPkgFromGhcjsPath = guessToolFromGhcjsPath ghcjsPkgProgram

guessHsc2hsFromGhcjsPath :: ConfiguredProgram -> Verbosity
                         -> ProgramSearchPath -> IO (Maybe FilePath)
guessHsc2hsFromGhcjsPath = guessToolFromGhcjsPath hsc2hsProgram

guessC2hsFromGhcjsPath :: ConfiguredProgram -> Verbosity
                       -> ProgramSearchPath -> IO (Maybe FilePath)
guessC2hsFromGhcjsPath = guessToolFromGhcjsPath c2hsProgram

guessHaddockFromGhcjsPath :: ConfiguredProgram -> Verbosity
                          -> ProgramSearchPath -> IO (Maybe FilePath)
guessHaddockFromGhcjsPath = guessToolFromGhcjsPath haddockProgram

guessToolFromGhcjsPath :: Program -> ConfiguredProgram
                       -> Verbosity -> ProgramSearchPath
                       -> IO (Maybe FilePath)
guessToolFromGhcjsPath tool ghcjsProg verbosity searchpath
  = do let toolname          = programName tool
           path              = programPath ghcjsProg
           dir               = takeDirectory path
           versionSuffix     = takeVersionSuffix (dropExeExtension path)
           guessNormal       = dir </> toolname <.> exeExtension
           guessGhcjsVersioned = dir </> (toolname ++ "-ghcjs" ++ versionSuffix)
                                 <.> exeExtension
           guessGhcjs        = dir </> (toolname ++ "-ghcjs")
                               <.> exeExtension
           guessVersioned    = dir </> (toolname ++ versionSuffix) <.> exeExtension
           guesses | null versionSuffix = [guessGhcjs, guessNormal]
                   | otherwise          = [guessGhcjsVersioned,
                                           guessGhcjs,
                                           guessVersioned,
                                           guessNormal]
       info verbosity $ "looking for tool " ++ toolname
         ++ " near compiler in " ++ dir
       exists <- mapM doesFileExist guesses
       case [ file | (file, True) <- zip guesses exists ] of
                   -- If we can't find it near ghc, fall back to the usual
                   -- method.
         []     -> programFindLocation tool verbosity searchpath
         (fp:_) -> do info verbosity $ "found " ++ toolname ++ " in " ++ fp
                      return (Just fp)

  where takeVersionSuffix :: FilePath -> String
        takeVersionSuffix = reverse . takeWhile (`elem ` "0123456789.-") .
                            reverse

        dropExeExtension :: FilePath -> FilePath
        dropExeExtension filepath =
          case splitExtension filepath of
            (filepath', extension) | extension == exeExtension -> filepath'
                                   | otherwise                 -> filepath


-- | Given a single package DB, return all installed packages.
getPackageDBContents :: Verbosity -> PackageDB -> ProgramConfiguration
                     -> IO PackageIndex
getPackageDBContents verbosity packagedb conf = do
  pkgss <- getInstalledPackages' verbosity [packagedb] conf
  toPackageIndex verbosity pkgss conf

-- | Given a package DB stack, return all installed packages.
getInstalledPackages :: Verbosity -> PackageDBStack -> ProgramConfiguration
                     -> IO PackageIndex
getInstalledPackages verbosity packagedbs conf = do
--  checkPackageDbEnvVar -- fixme drop support for GHCJS_PACKAGE_PATH in ghcjs-pkg
  checkPackageDbStack packagedbs
  pkgss <- getInstalledPackages' verbosity packagedbs conf
  index <- toPackageIndex verbosity pkgss conf
  return $! index

toPackageIndex :: Verbosity
               -> [(PackageDB, [InstalledPackageInfo])]
               -> ProgramConfiguration
               -> IO PackageIndex
toPackageIndex verbosity pkgss conf = do
  -- On Windows, various fields have $topdir/foo rather than full
  -- paths. We need to substitute the right value in so that when
  -- we, for example, call gcc, we have proper paths to give it.
  topDir <- ghcjsLibDir' verbosity ghcjsProg
  let indices = [ PackageIndex.fromList (map (substTopDir topDir) pkgs)
                | (_, pkgs) <- pkgss ]
  return $! (mconcat indices)

  where
    Just ghcjsProg = lookupProgram ghcjsProgram conf

substTopDir :: FilePath -> InstalledPackageInfo -> InstalledPackageInfo
substTopDir topDir ipo
 = ipo {
       InstalledPackageInfo.importDirs
           = map f (InstalledPackageInfo.importDirs ipo),
       InstalledPackageInfo.libraryDirs
           = map f (InstalledPackageInfo.libraryDirs ipo),
       InstalledPackageInfo.includeDirs
           = map f (InstalledPackageInfo.includeDirs ipo),
       InstalledPackageInfo.frameworkDirs
           = map f (InstalledPackageInfo.frameworkDirs ipo),
       InstalledPackageInfo.haddockInterfaces
           = map f (InstalledPackageInfo.haddockInterfaces ipo),
       InstalledPackageInfo.haddockHTMLs
           = map f (InstalledPackageInfo.haddockHTMLs ipo)
   }
    where f ('$':'t':'o':'p':'d':'i':'r':rest) = topDir ++ rest
          f x = x

checkPackageDbStack :: PackageDBStack -> IO ()
checkPackageDbStack (GlobalPackageDB:rest)
  | GlobalPackageDB `notElem` rest = return ()
checkPackageDbStack rest
  | GlobalPackageDB `notElem` rest =
  die $ "With current ghc versions the global package db is always used "
     ++ "and must be listed first. This ghc limitation may be lifted in "
     ++ "future, see http://hackage.haskell.org/trac/ghc/ticket/5977"
checkPackageDbStack _ =
  die $ "If the global package db is specified, it must be "
     ++ "specified first and cannot be specified multiple times"

getInstalledPackages' :: Verbosity -> [PackageDB] -> ProgramConfiguration
                      -> IO [(PackageDB, [InstalledPackageInfo])]
getInstalledPackages' verbosity packagedbs conf =
  sequence
    [ do pkgs <- HcPkg.dump verbosity ghcjsPkgProg packagedb
         return (packagedb, pkgs)
    | packagedb <- packagedbs ]
  where
    Just ghcjsPkgProg = lookupProgram  ghcjsPkgProgram conf
    Just ghcjsProg    = lookupProgram  ghcjsProgram conf
    Just ghcjsVersion = programVersion ghcjsProg


ghcjsLibDir :: Verbosity -> LocalBuildInfo -> IO FilePath
ghcjsLibDir verbosity lbi =
    (reverse . dropWhile isSpace . reverse) `fmap`
     rawSystemProgramStdoutConf verbosity ghcjsProgram
     (withPrograms lbi) ["--print-libdir"]

ghcjsLibDir' :: Verbosity -> ConfiguredProgram -> IO FilePath
ghcjsLibDir' verbosity ghcjsProg =
    (reverse . dropWhile isSpace . reverse) `fmap`
     rawSystemProgramStdout verbosity ghcjsProg ["--print-libdir"]

buildLib, replLib :: Verbosity
         -> Cabal.Flag (Maybe Int)
         -> PackageDescription
         -> LocalBuildInfo
         -> Library
         -> ComponentLocalBuildInfo
         -> IO ()
buildLib = buildOrReplLib False
replLib  = buildOrReplLib True

buildOrReplLib :: Bool -> Verbosity  -> Cabal.Flag (Maybe Int)
               -> PackageDescription -> LocalBuildInfo
               -> Library            -> ComponentLocalBuildInfo -> IO ()
buildOrReplLib forRepl verbosity numJobsFlag pkg_descr lbi lib clbi = do
  libName <- case componentLibraries clbi of
             [libName] -> return libName
             [] -> die "No library name found when building library"
             _  -> die "Multiple library names found when building library"

  let libTargetDir = buildDir lbi
      numJobs = fromMaybe 1 $ fromFlagOrDefault Nothing numJobsFlag
      pkgid = packageId pkg_descr
      whenVanillaLib forceVanilla =
        when (not forRepl && (forceVanilla || withVanillaLib lbi))
      whenProfLib = when (not forRepl && withProfLib lbi)
      whenSharedLib forceShared =
        when (not forRepl &&  (forceShared || withSharedLib lbi))
      whenGHCiLib = when (not forRepl && withGHCiLib lbi && withVanillaLib lbi)
      ifReplLib = when forRepl
      comp = compiler lbi
      ghcjsVersion = compilerVersion comp
      nativeToo = ghcjsNativeToo comp
      (Platform _hostArch hostOS) = hostPlatform lbi

  (ghcjsProg, _) <- requireProgram verbosity ghcjsProgram (withPrograms lbi)
  let runGhcjsProg        = runGHC verbosity ghcjsProg comp
      libBi               = libBuildInfo lib
      isGhcjsDynamic      = ghcjsDynamic comp
      dynamicTooSupported = ghcjsSupportsDynamicToo comp
      doingTH = EnableExtension TemplateHaskell `elem` allExtensions libBi
      forceVanillaLib = doingTH && not isGhcjsDynamic
      forceSharedLib  = doingTH &&     isGhcjsDynamic
      -- TH always needs default libs, even when building for profiling

  createDirectoryIfMissingVerbose verbosity True libTargetDir
  -- TODO: do we need to put hs-boot files into place for mutually recursive
  -- modules?
  let cObjs       = map (`replaceExtension` objExtension) (cSources libBi)
      baseOpts    = componentGhcOptions verbosity lbi libBi clbi libTargetDir
      vanillaOpts = baseOpts `mappend` mempty {
                      ghcOptMode         = toFlag GhcModeMake,
                      ghcOptNumJobs      = toFlag numJobs,
                      ghcOptPackageName  = toFlag pkgid,
                      ghcOptInputModules = libModules lib
                    }

      profOpts    = vanillaOpts `mappend` mempty {
                      ghcOptProfilingMode = toFlag True,
                      ghcOptHiSuffix      = toFlag "p_hi",
                      ghcOptObjSuffix     = toFlag "p_o",
                      ghcOptExtra         = ghcProfOptions libBi
                    }

      sharedOpts  = vanillaOpts `mappend` mempty {
                      ghcOptDynLinkMode = toFlag GhcDynamicOnly,
                      ghcOptFPic        = toFlag True,
                      ghcOptHiSuffix    = toFlag "dyn_hi",
                      ghcOptObjSuffix   = toFlag "dyn_o",
                      ghcOptExtra       = ghcSharedOptions libBi
                    }
      linkerOpts = mempty {
                      ghcOptLinkOptions    = PD.ldOptions libBi,
                      ghcOptLinkLibs       = extraLibs libBi,
                      ghcOptLinkLibPath    = extraLibDirs libBi,
                      ghcOptLinkFrameworks = PD.frameworks libBi,
                      ghcOptInputFiles     = [libTargetDir </> x | x <- cObjs]
                   }
      replOpts    = vanillaOpts {
                      ghcOptExtra        = filterGhciFlags
                                           (ghcOptExtra vanillaOpts),
                      ghcOptNumJobs      = mempty
                    }
                    `mappend` linkerOpts
                    `mappend` mempty {
                      ghcOptMode         = toFlag GhcModeInteractive,
                      ghcOptOptimisation = toFlag GhcNoOptimisation
                    }

      vanillaSharedOpts = vanillaOpts `mappend` mempty {
                      ghcOptDynLinkMode  = toFlag GhcStaticAndDynamic,
                      ghcOptDynHiSuffix  = toFlag "dyn_hi",
                      ghcOptDynObjSuffix = toFlag "dyn_o"
                    }

  unless (null (libModules lib)) $
    do let vanilla = whenVanillaLib forceVanillaLib (runGhcjsProg vanillaOpts)
           shared  = whenSharedLib  forceSharedLib  (runGhcjsProg sharedOpts)
           useDynToo = dynamicTooSupported &&
                       (forceVanillaLib || withVanillaLib lbi) &&
                       (forceSharedLib  || withSharedLib  lbi) &&
                       null (ghcSharedOptions libBi)
       if useDynToo
           then runGhcjsProg vanillaSharedOpts
           else if isGhcjsDynamic then do shared;  vanilla
                                else do vanilla; shared
       whenProfLib (runGhcjsProg profOpts)

  -- build any C sources
  unless (null (cSources libBi) || not nativeToo) $ do
     info verbosity "Building C Sources..."
     sequence_
       [ do let vanillaCcOpts = (componentCcGhcOptions verbosity lbi
                                    libBi clbi libTargetDir filename)
                profCcOpts    = vanillaCcOpts `mappend` mempty {
                                  ghcOptProfilingMode = toFlag True,
                                  ghcOptObjSuffix     = toFlag "p_o"
                                }
                sharedCcOpts  = vanillaCcOpts `mappend` mempty {
                                  ghcOptFPic        = toFlag True,
                                  ghcOptDynLinkMode = toFlag GhcDynamicOnly,
                                  ghcOptObjSuffix   = toFlag "dyn_o"
                                }
                odir          = fromFlag (ghcOptObjDir vanillaCcOpts)
            createDirectoryIfMissingVerbose verbosity True odir
            runGhcjsProg vanillaCcOpts
            whenSharedLib forceSharedLib (runGhcjsProg sharedCcOpts)
            whenProfLib (runGhcjsProg profCcOpts)
       | filename <- cSources libBi]

  -- TODO: problem here is we need the .c files built first, so we can load them
  -- with ghci, but .c files can depend on .h files generated by ghc by ffi
  -- exports.
  unless (null (libModules lib)) $
     ifReplLib (runGhcjsProg replOpts)


  -- link:
  when nativeToo $ do
    info verbosity "Linking..."
    let cProfObjs   = map (`replaceExtension` ("p_" ++ objExtension))
                      (cSources libBi)
        cSharedObjs = map (`replaceExtension` ("dyn_" ++ objExtension))
                      (cSources libBi)
        cid = compilerId (compiler lbi)
        vanillaLibFilePath = libTargetDir </> mkLibName           libName
        profileLibFilePath = libTargetDir </> mkProfLibName       libName
        sharedLibFilePath  = libTargetDir </> mkSharedLibName cid libName
        ghciLibFilePath    = libTargetDir </> mkGHCiLibName       libName
        libInstallPath = libdir $ absoluteInstallDirs pkg_descr lbi NoCopyDest
        sharedLibInstallPath = libInstallPath </> mkSharedLibName cid libName

    hObjs     <- getHaskellObjects lib lbi
                      libTargetDir objExtension True
    hProfObjs <-
      if (withProfLib lbi)
              then getHaskellObjects lib lbi
                      libTargetDir ("p_" ++ objExtension) True
              else return []
    hSharedObjs <-
      if (withSharedLib lbi)
              then getHaskellObjects lib lbi
                      libTargetDir ("dyn_" ++ objExtension) False
              else return []

    unless (null hObjs && null cObjs) $ do

      let staticObjectFiles =
                 hObjs
              ++ map (libTargetDir </>) cObjs
          profObjectFiles =
                 hProfObjs
              ++ map (libTargetDir </>) cProfObjs
          ghciObjFiles =
                 hObjs
              ++ map (libTargetDir </>) cObjs
          dynamicObjectFiles =
                 hSharedObjs
              ++ map (libTargetDir </>) cSharedObjs
          -- After the relocation lib is created we invoke ghc -shared
          -- with the dependencies spelled out as -package arguments
          -- and ghc invokes the linker with the proper library paths
          ghcSharedLinkArgs =
              mempty {
                ghcOptShared             = toFlag True,
                ghcOptDynLinkMode        = toFlag GhcDynamicOnly,
                ghcOptInputFiles         = dynamicObjectFiles,
                ghcOptOutputFile         = toFlag sharedLibFilePath,
                ghcOptPackageName        = toFlag pkgid,
                ghcOptNoAutoLinkPackages = toFlag True,
                ghcOptPackageDBs         = withPackageDB lbi,
                ghcOptPackages           = componentPackageDeps clbi,
                ghcOptLinkLibs           = extraLibs libBi,
                ghcOptLinkLibPath        = extraLibDirs libBi
              }

      whenVanillaLib False $ do
        Ar.createArLibArchive verbosity lbi vanillaLibFilePath staticObjectFiles

      whenProfLib $ do
        Ar.createArLibArchive verbosity lbi profileLibFilePath profObjectFiles

      whenGHCiLib $ do
        (ldProg, _) <- requireProgram verbosity ldProgram (withPrograms lbi)
        Ld.combineObjectFiles verbosity ldProg
          ghciLibFilePath ghciObjFiles

      whenSharedLib False $
        runGhcjsProg ghcSharedLinkArgs

-- when using -split-objs, we need to search for object files in the
-- Module_split directory for each module.
getHaskellObjects :: Library -> LocalBuildInfo
                  -> FilePath -> String -> Bool -> IO [FilePath]
getHaskellObjects lib lbi pref wanted_obj_ext allow_split_objs
  | splitObjs lbi && allow_split_objs = do
        let splitSuffix = "_" ++ wanted_obj_ext ++ "_split"
            dirs = [ pref </> (ModuleName.toFilePath x ++ splitSuffix)
                   | x <- libModules lib ]
        objss <- mapM getDirectoryContents dirs
        let objs = [ dir </> obj
                   | (objs',dir) <- zip objss dirs, obj <- objs',
                     let obj_ext = takeExtension obj,
                     '.':wanted_obj_ext == obj_ext ]
        return objs
  | otherwise  =
        return [ pref </> ModuleName.toFilePath x <.> wanted_obj_ext
               | x <- libModules lib ]

buildExe, replExe :: Verbosity          -> Cabal.Flag (Maybe Int)
                  -> PackageDescription -> LocalBuildInfo
                  -> Executable         -> ComponentLocalBuildInfo -> IO ()
buildExe = buildOrReplExe False
replExe  = buildOrReplExe True

buildOrReplExe :: Bool -> Verbosity  -> Cabal.Flag (Maybe Int)
               -> PackageDescription -> LocalBuildInfo
               -> Executable         -> ComponentLocalBuildInfo -> IO ()
buildOrReplExe forRepl verbosity numJobsFlag _pkg_descr lbi
  exe@Executable { exeName = exeName', modulePath = modPath } clbi = do

  (ghcjsProg, _) <- requireProgram verbosity ghcjsProgram (withPrograms lbi)
  let comp         = compiler lbi
      numJobs      = fromMaybe 1 $
                     fromFlagOrDefault Nothing numJobsFlag
      runGhcjsProg = runGHC verbosity ghcjsProg comp
      exeBi        = buildInfo exe

  -- exeNameReal, the name that GHC really uses (with .exe on Windows)
  let exeNameReal = exeName' <.>
                    (if takeExtension exeName' /= ('.':exeExtension)
                       then exeExtension
                       else "")

  let targetDir = (buildDir lbi) </> exeName'
  let exeDir    = targetDir </> (exeName' ++ "-tmp")
  createDirectoryIfMissingVerbose verbosity True targetDir
  createDirectoryIfMissingVerbose verbosity True exeDir
  -- TODO: do we need to put hs-boot files into place for mutually recursive
  -- modules?  FIX: what about exeName.hi-boot?

  -- build executables

  srcMainFile         <- findFile (exeDir : hsSourceDirs exeBi) modPath
  let isGhcjsDynamic      = ghcjsDynamic comp
      dynamicTooSupported = ghcjsSupportsDynamicToo comp
      isHaskellMain = elem (takeExtension srcMainFile) [".hs", ".lhs"]
      cSrcs         = cSources exeBi ++ [srcMainFile | not isHaskellMain]
      cObjs         = map (`replaceExtension` objExtension) cSrcs
      nativeToo     = ghcjsNativeToo comp
      baseOpts   = (componentGhcOptions verbosity lbi exeBi clbi exeDir)
                    `mappend` mempty {
                      ghcOptMode         = toFlag GhcModeMake,
                      ghcOptInputFiles   =
                        [ srcMainFile | isHaskellMain],
                      ghcOptInputModules =
                        [ m | not isHaskellMain, m <- exeModules exe]
                    }
      staticOpts = baseOpts `mappend` mempty {
                      ghcOptDynLinkMode    = toFlag GhcStaticOnly
                   }
      profOpts   = baseOpts `mappend` mempty {
                      ghcOptProfilingMode  = toFlag True,
                      ghcOptHiSuffix       = toFlag "p_hi",
                      ghcOptObjSuffix      = toFlag "p_o",
                      ghcOptExtra          = ghcProfOptions exeBi
                    }
      dynOpts    = baseOpts `mappend` mempty {
                      ghcOptDynLinkMode    = toFlag GhcDynamicOnly,
                      ghcOptHiSuffix       = toFlag "dyn_hi",
                      ghcOptObjSuffix      = toFlag "dyn_o",
                      ghcOptExtra          = ghcSharedOptions exeBi
                    }
      dynTooOpts = staticOpts `mappend` mempty {
                      ghcOptDynLinkMode    = toFlag GhcStaticAndDynamic,
                      ghcOptDynHiSuffix    = toFlag "dyn_hi",
                      ghcOptDynObjSuffix   = toFlag "dyn_o"
                    }
      linkerOpts = mempty {
                      ghcOptLinkOptions    = PD.ldOptions exeBi,
                      ghcOptLinkLibs       = extraLibs exeBi,
                      ghcOptLinkLibPath    = extraLibDirs exeBi,
                      ghcOptLinkFrameworks = PD.frameworks exeBi,
                      ghcOptInputFiles     = [exeDir </> x | x <- cObjs]
                   }
      replOpts   = baseOpts {
                      ghcOptExtra          = filterGhciFlags
                                             (ghcOptExtra baseOpts)
                   }
                   -- For a normal compile we do separate invocations of ghc for
                   -- compiling as for linking. But for repl we have to do just
                   -- the one invocation, so that one has to include all the
                   -- linker stuff too, like -l flags and any .o files from C
                   -- files etc.
                   `mappend` linkerOpts
                   `mappend` mempty {
                      ghcOptMode           = toFlag GhcModeInteractive,
                      ghcOptOptimisation   = toFlag GhcNoOptimisation
                   }
      commonOpts  | withProfExe lbi = profOpts
                  | withDynExe  lbi = dynOpts
                  | otherwise       = staticOpts
      compileOpts | useDynToo = dynTooOpts
                  | otherwise = commonOpts
      withStaticExe = (not $ withProfExe lbi) && (not $ withDynExe lbi)

      -- For building exe's that use TH with -prof or -dynamic we actually have
      -- to build twice, once without -prof/-dynamic and then again with
      -- -prof/-dynamic. This is because the code that TH needs to run at
      -- compile time needs to be the vanilla ABI so it can be loaded up and run
      -- by the compiler.
      -- With dynamic-by-default GHC the TH object files loaded at compile-time
      -- need to be .dyn_o instead of .o.
      doingTH = EnableExtension TemplateHaskell `elem` allExtensions exeBi
      -- Should we use -dynamic-too instead of compiling twice?
      useDynToo = dynamicTooSupported && isGhcjsDynamic
                  && doingTH && withStaticExe && null (ghcSharedOptions exeBi)
      compileTHOpts | isGhcjsDynamic = dynOpts
                    | otherwise      = staticOpts
      compileForTH
        | forRepl      = False
        | useDynToo    = False
        | isGhcjsDynamic = doingTH && (withProfExe lbi || withStaticExe)
        | otherwise      = doingTH && (withProfExe lbi || withDynExe lbi)

      linkOpts = commonOpts `mappend`
                 linkerOpts `mappend` mempty {
                      ghcOptLinkNoHsMain   = toFlag (not isHaskellMain)
                 }

  -- Build static/dynamic object files for TH, if needed.
  when compileForTH $
    runGhcjsProg compileTHOpts { ghcOptNoLink  = toFlag True
                               , ghcOptNumJobs = toFlag numJobs }

  unless forRepl $
    runGhcjsProg compileOpts { ghcOptNoLink  = toFlag True
                             , ghcOptNumJobs = toFlag numJobs }

  -- build any C sources
  unless (null cSrcs || not nativeToo) $ do
   info verbosity "Building C Sources..."
   sequence_
     [ do let opts = (componentCcGhcOptions verbosity lbi exeBi clbi
                         exeDir filename) `mappend` mempty {
                       ghcOptDynLinkMode   = toFlag (if withDynExe lbi
                                                       then GhcDynamicOnly
                                                       else GhcStaticOnly),
                       ghcOptProfilingMode = toFlag (withProfExe lbi)
                     }
              odir = fromFlag (ghcOptObjDir opts)
          createDirectoryIfMissingVerbose verbosity True odir
          runGhcjsProg opts
     | filename <- cSrcs ]

  -- TODO: problem here is we need the .c files built first, so we can load them
  -- with ghci, but .c files can depend on .h files generated by ghc by ffi
  -- exports.
  when forRepl $ runGhcjsProg replOpts

  -- link:
  unless forRepl $ do
    info verbosity "Linking..."
    runGhcjsProg linkOpts { ghcOptOutputFile = toFlag (targetDir </> exeNameReal) }

-- |Install for ghc, .hi, .a and, if --with-ghci given, .o
installLib    :: Verbosity
              -> LocalBuildInfo
              -> FilePath  -- ^install location
              -> FilePath  -- ^install location for dynamic libraries
              -> FilePath  -- ^Build location
              -> PackageDescription
              -> Library
              -> ComponentLocalBuildInfo
              -> IO ()
installLib verbosity lbi targetDir dynlibTargetDir builtDir _pkg lib clbi = do
  whenVanilla $ mapM_ copyModuleFiles ["js_hi", "js_o"]
  whenProf    $ mapM_ copyModuleFiles ["js_p_hi", "js_p_o"]
  whenShared  $ mapM_ copyModuleFiles ["js_dyn_hi", "js_dyn_o"]

  when (ghcjsNativeToo $ compiler lbi) $ do
    -- copy .hi files over:
    whenVanilla $ copyModuleFiles "hi"
    whenProf    $ copyModuleFiles "p_hi"
    whenShared  $ copyModuleFiles "dyn_hi"

    -- copy the built library files over:
    whenVanilla $ mapM_ (installOrdinary builtDir targetDir)       vanillaLibNames
    whenProf    $ mapM_ (installOrdinary builtDir targetDir)       profileLibNames
    whenGHCi    $ mapM_ (installOrdinary builtDir targetDir)       ghciLibNames
    whenShared  $ mapM_ (installShared   builtDir dynlibTargetDir) sharedLibNames

  where
    install isShared srcDir dstDir name = do
      let src = srcDir </> name
          dst = dstDir </> name
      createDirectoryIfMissingVerbose verbosity True dstDir
      if isShared
        then do when (stripLibs lbi) $ Strip.stripLib verbosity
                                       (hostPlatform lbi) (withPrograms lbi) src
                installExecutableFile verbosity src dst
        else installOrdinaryFile   verbosity src dst

    installOrdinary = install False
    installShared   = install True

    copyModuleFiles ext =
      findModuleFiles [builtDir] [ext] (libModules lib)
      >>= installOrdinaryFiles verbosity targetDir

    cid = compilerId (compiler lbi)
    libNames = componentLibraries clbi
    vanillaLibNames = map mkLibName             libNames
    profileLibNames = map mkProfLibName         libNames
    ghciLibNames    = map mkGHCiLibName         libNames
    sharedLibNames  = map (mkSharedLibName cid) libNames

    hasLib    = not $ null (libModules lib)
                   && null (cSources (libBuildInfo lib))
    whenVanilla = when (hasLib && withVanillaLib lbi)
    whenProf    = when (hasLib && withProfLib    lbi)
    whenGHCi    = when (hasLib && withGHCiLib    lbi)
    whenShared  = when (hasLib && withSharedLib  lbi)

installExe :: Verbosity
              -> LocalBuildInfo
              -> InstallDirs FilePath
              -> FilePath
              -> (FilePath, FilePath)
              -> PackageDescription
              -> Executable
              -> IO ()
installExe verbosity lbi installDirs buildPref
  (progprefix, progsuffix) _pkg exe = do
  let binDir = bindir installDirs
  createDirectoryIfMissingVerbose verbosity True binDir
  let exeFileName = exeName exe
      fixedExeBaseName = progprefix ++ exeName exe ++ progsuffix
      installBinary dest = do
          rawSystemProgramConf verbosity ghcjsProgram (withPrograms lbi) $
            [ "--install-executable"
            , buildPref </> exeName exe </> exeFileName
            , "-o", dest
            ] ++ case (stripExes lbi, lookupProgram stripProgram $ withPrograms lbi) of
                    (True, Just strip) -> ["--strip-program=" ++ programPath strip]
                    _ -> []
  installBinary (binDir </> fixedExeBaseName)

libAbiHash :: Verbosity -> PackageDescription -> LocalBuildInfo
           -> Library -> ComponentLocalBuildInfo -> IO String
libAbiHash verbosity pkg_descr lbi lib clbi = do
  let
      libBi       = libBuildInfo lib
      comp        = compiler lbi
      vanillaArgs =
        (componentGhcOptions verbosity lbi libBi clbi (buildDir lbi))
        `mappend` mempty {
          ghcOptMode         = toFlag GhcModeAbiHash,
          ghcOptPackageName  = toFlag (packageId pkg_descr),
          ghcOptInputModules = exposedModules lib
        }
      profArgs = vanillaArgs `mappend` mempty {
                     ghcOptProfilingMode = toFlag True,
                     ghcOptHiSuffix      = toFlag "js_p_hi",
                     ghcOptObjSuffix     = toFlag "js_p_o",
                     ghcOptExtra         = ghcProfOptions libBi
                 }
      ghcArgs = if withVanillaLib lbi then vanillaArgs
           else if withProfLib    lbi then profArgs
           else error "libAbiHash: Can't find an enabled library way"
  --
  (ghcjsProg, _) <- requireProgram verbosity ghcjsProgram (withPrograms lbi)
  getProgramInvocationOutput verbosity (ghcInvocation ghcjsProg comp ghcArgs)


registerPackage :: Verbosity
                -> InstalledPackageInfo
                -> PackageDescription
                -> LocalBuildInfo
                -> Bool
                -> PackageDBStack
                -> IO ()
registerPackage verbosity installedPkgInfo _pkg lbi _inplace packageDbs = do
  let Just ghcjsPkg = lookupProgram ghcjsPkgProgram (withPrograms lbi)
  HcPkg.reregister verbosity ghcjsPkg packageDbs (Right installedPkgInfo)

componentGhcOptions :: Verbosity -> LocalBuildInfo
                    -> BuildInfo -> ComponentLocalBuildInfo -> FilePath
                    -> GhcOptions
componentGhcOptions verbosity lbi bi clbi odir =
    mempty {
      ghcOptVerbosity       = toFlag verbosity,
      ghcOptHideAllPackages = toFlag True,
      ghcOptCabal           = toFlag True,
      ghcOptPackageDBs      = withPackageDB lbi,
      ghcOptPackages        = componentPackageDeps clbi,
      ghcOptSplitObjs       = toFlag (splitObjs lbi),
      ghcOptSourcePathClear = toFlag True,
      ghcOptSourcePath      = [odir] ++ nub (hsSourceDirs bi)
                                    ++ [autogenModulesDir lbi],
      ghcOptCppIncludePath  = [autogenModulesDir lbi, odir]
                                    ++ PD.includeDirs bi,
      ghcOptCppOptions      = cppOptions bi,
      ghcOptCppIncludes     = [autogenModulesDir lbi </> cppHeaderName],
      ghcOptFfiIncludes     = PD.includes bi,
      ghcOptObjDir          = toFlag odir,
      ghcOptHiDir           = toFlag odir,
      ghcOptStubDir         = toFlag odir,
      ghcOptOutputDir       = toFlag odir,
      ghcOptOptimisation    = toGhcOptimisation (withOptimization lbi),
      ghcOptExtra           = hcOptions GHC bi,
      ghcOptLanguage        = toFlag (fromMaybe Haskell98 (defaultLanguage bi)),
      -- Unsupported extensions have already been checked by configure
      ghcOptExtensions      = usedExtensions bi,
      ghcOptExtensionMap    = compilerExtensions (compiler lbi)
    }
  where
    toGhcOptimisation NoOptimisation      = mempty --TODO perhaps override?
    toGhcOptimisation NormalOptimisation  = toFlag GhcNormalOptimisation
    toGhcOptimisation MaximumOptimisation = toFlag GhcMaximumOptimisation


componentCcGhcOptions :: Verbosity -> LocalBuildInfo
                      -> BuildInfo -> ComponentLocalBuildInfo
                      -> FilePath -> FilePath
                      -> GhcOptions
componentCcGhcOptions verbosity lbi bi clbi odir filename =
    mempty {
      ghcOptVerbosity      = toFlag verbosity,
      ghcOptMode           = toFlag GhcModeCompile,
      ghcOptInputFiles     = [filename],

      ghcOptCppIncludePath = [autogenModulesDir lbi, odir]
                                   ++ PD.includeDirs bi,
      ghcOptPackageDBs     = withPackageDB lbi,
      ghcOptPackages       = componentPackageDeps clbi,
      ghcOptCcOptions      = (case withOptimization lbi of
                                  NoOptimisation -> []
                                  _              -> ["-O2"]) ++
                                  PD.ccOptions bi,
      ghcOptObjDir         = toFlag odir
    }

ghcjsLookupProperty :: String -> Compiler -> Bool
ghcjsLookupProperty prop comp =
  case M.lookup prop (compilerProperties comp) of
    Just "YES" -> True
    _          -> False

ghcjsDynamic :: Compiler -> Bool
ghcjsDynamic = ghcjsLookupProperty "GHC Dynamic"

ghcjsSupportsDynamicToo :: Compiler -> Bool
ghcjsSupportsDynamicToo = ghcjsLookupProperty "Support dynamic-too"

findGhcjsGhcVersion :: Verbosity -> FilePath -> IO (Maybe Version)
findGhcjsGhcVersion verbosity pgm =
  findProgramVersion "--numeric-ghc-version" id verbosity pgm

findGhcjsPkgGhcVersion :: Verbosity -> FilePath -> IO (Maybe Version)
findGhcjsPkgGhcVersion verbosity pgm =
  findProgramVersion "--numeric-ghc-version" id verbosity pgm

-- | When we call the underlying GHC module, report the version
--   of the GHC that we've been built with
withGhcVersion :: Verbosity -> ConfiguredProgram -> IO ConfiguredProgram
withGhcVersion verbosity pgm
  | programId pgm == "ghcjs" = do
      ver <- findGhcjsGhcVersion verbosity (programPath pgm)
      return $ pgm { programVersion = ver }
  | programId pgm == "ghcjs-pkg" = do
      ver <- findGhcjsPkgGhcVersion verbosity (programPath pgm)
      return $ pgm { programVersion = ver }
  | otherwise = return pgm

-- | Adjust our reported version number number to match the underlying
--   GHC's for calling the Distribution.Simple.GHC module
makeGhcLbi :: Bool -> LocalBuildInfo -> LocalBuildInfo
makeGhcLbi ghcFlavour lbi =
  let c = compiler lbi
      CompilerId _ _ (Just cid@(CompilerId _ ghcVer _ )) = compilerId c
      cid' = CompilerId GHCJS ghcVer Nothing
  in  if ghcFlavour
        then lbi { compiler = c { compilerId = cid } }
        else lbi { compiler = c { compilerId = cid' } }


-- -----------------------------------------------------------------------------
-- Registering

-- | Create an empty package DB at the specified location.
initPackageDB :: Verbosity -> ProgramConfiguration -> FilePath -> IO ()
initPackageDB verbosity conf dbPath = HcPkg.init verbosity ghcjsPkgProg dbPath
  where
    Just ghcjsPkgProg = lookupProgram ghcjsPkgProgram conf

-- | Run 'ghcjs-pkg' using a given package DB stack, directly forwarding the
-- provided command-line arguments to it.
invokeHcPkg :: Verbosity -> ProgramConfiguration -> PackageDBStack -> [String]
               -> IO ()
invokeHcPkg verbosity conf dbStack extraArgs =
    HcPkg.invoke verbosity ghcjsPkgProg dbStack extraArgs
  where
    Just ghcjsPkgProg = lookupProgram ghcjsPkgProgram conf

-- | Strip out flags that are not supported in ghci
filterGhciFlags :: [String] -> [String]
filterGhciFlags = filter supported
  where
    supported ('-':'O':_) = False
    supported "-debug"    = False
    supported "-threaded" = False
    supported "-ticky"    = False
    supported "-eventlog" = False
    supported "-prof"     = False
    supported "-unreg"    = False
    supported _           = True

mkGHCiLibName :: LibraryName -> String
mkGHCiLibName (LibraryName lib) = lib <.> "o"
