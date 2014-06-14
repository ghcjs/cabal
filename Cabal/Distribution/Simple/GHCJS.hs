module Distribution.Simple.GHCJS (
        configure, getInstalledPackages,
        getPackageDBContents,
        buildLib, buildExe,
        installLib, installExe,
        libAbiHash,
        initPackageDB,
        registerPackage,
        componentGhcOptions,
        ghcLibDir,
        ghcDynamic,
        invokeHcPkg
  ) where

import Control.Monad ( unless, when )
import Distribution.PackageDescription as PD
         ( PackageDescription, BuildInfo(..), Executable(..)
         , Library(..), buildInfo, libBuildInfo, libModules )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), CompilerId(..), Compiler(..)
         , PackageDBStack, PackageDB(..), Flag )
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(..), ComponentLocalBuildInfo(..) )
import Distribution.Simple.InstallDirs hiding ( absoluteInstallDirs )
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Simple.Program
         ( Program(..), ConfiguredProgram(..), ProgramConfiguration
         , ProgramSearchPath, rawSystemProgramConf
         , requireProgramVersion
         , userMaybeSpecifyPath, programPath
         , addKnownPrograms, updateProgram, lookupProgram
         , ghcjsProgram, ghcjsPkgProgram, hsc2hsProgram
         , c2hsProgram, stripProgram
         )
import qualified Distribution.Simple.Program.HcPkg as HcPkg
import Distribution.Simple.Program.GHC ( GhcOptions )
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
import System.Directory ( doesDirectoryExist, doesFileExist )
import System.FilePath  ( (</>), (<.>), takeDirectory, splitExtension, splitFileName )
import qualified Data.Map as M
import qualified Distribution.Simple.GHC.Base as Base
import qualified Distribution.Simple.GHC as GHC

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

      ghcProg' = ghcjsProg { programId      = "ghc"
                           , programVersion = Just ghcjsGhcVersion
                           }
      ghcPkgProg' = ghcjsPkgProg
                           { programId      = "ghc-pkg"
                           , programVersion =  Just ghcjsPkgGhcVersion
                           }

      conf3 = updateProgram ghcProg'
            $ updateProgram ghcPkgProg'
            $ addKnownPrograms [ hsc2hsProgram', c2hsProgram', haddockProgram' ] conf2

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
  let conf4      = Base.configureToolchain ghcjsProg' ghcInfoMap conf3
  return (comp, compPlatform, conf4)

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
getPackageDBContents verbosity packagedb conf =
  GHC.getPackageDBContents verbosity packagedb conf

getInstalledPackages :: Verbosity -> PackageDBStack -> ProgramConfiguration
                     -> IO PackageIndex
getInstalledPackages verbosity packagedbs conf =
  GHC.getInstalledPackages verbosity packagedbs conf

ghcLibDir :: Verbosity -> LocalBuildInfo -> IO FilePath
ghcLibDir verbosity lbi =
  GHC.ghcLibDir verbosity (makeGhcLbi False lbi)

buildLib :: Verbosity
         -> Cabal.Flag (Maybe Int)
         -> PackageDescription
         -> LocalBuildInfo
         -> Library
         -> ComponentLocalBuildInfo
         -> IO ()
buildLib verbosity numJobsFlag pkg_descr lbi lib clbi = do
  let targetDir = buildDir lbi </> "js"
      sources   = map splitFileName (jsSources (libBuildInfo lib))
  unless (null sources) $ do
    info verbosity "Copying JavaScript Sources..."
    copyFiles verbosity targetDir sources
  GHC.buildLib verbosity numJobsFlag pkg_descr (makeGhcLbi False lbi) lib clbi

buildExe :: Verbosity
         -> Cabal.Flag (Maybe Int)
         -> PackageDescription
         -> LocalBuildInfo
         -> Executable
         -> ComponentLocalBuildInfo
         -> IO ()
buildExe verbosity numJobsFlag pkg_descr lbi exe clbi =
  GHC.buildExeWith (jsSources (buildInfo exe))
    verbosity numJobsFlag pkg_descr (makeGhcLbi False lbi) exe clbi

installLib :: Verbosity
           -> LocalBuildInfo
           -> FilePath  -- ^install location
           -> FilePath  -- ^install location for dynamic librariess
           -> FilePath  -- ^Build location
           -> PackageDescription
           -> Library
           -> ComponentLocalBuildInfo
           -> IO ()
installLib verbosity lbi targetDir dynlibTargetDir builtDir pkg lib clbi = do
  GHC.installLib verbosity (makeGhcLbi False lbi) targetDir dynlibTargetDir builtDir pkg lib clbi
  let jsDir = builtDir </> "js"
  hasJsFiles <- doesDirectoryExist jsDir
  when hasJsFiles $
    installDirectoryContents verbosity jsDir (targetDir </> "js")
  whenVanilla $ mapM_ copyModuleFiles ["js_hi", "js_o"]
  whenProf    $ mapM_ copyModuleFiles ["js_p_hi", "js_p_o"]
  whenShared  $ mapM_ copyModuleFiles ["js_dyn_hi", "js_dyn_o"]
    where
      hasLib    = not $ null (libModules lib)
                     && null (cSources (libBuildInfo lib))
      copyModuleFiles ext =
        findModuleFiles [builtDir] [ext] (libModules lib)
          >>= installOrdinaryFiles verbosity targetDir
      whenVanilla = when (hasLib && withVanillaLib lbi)
      whenProf    = when (hasLib && withProfLib    lbi)
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

libAbiHash :: Verbosity
           -> PackageDescription
           -> LocalBuildInfo
           -> Library
           -> ComponentLocalBuildInfo
           -> IO String
libAbiHash verbosity pkg_descr lbi lib clbi = do
  GHC.libAbiHash verbosity pkg_descr (makeGhcLbi False lbi) lib clbi

registerPackage :: Verbosity
                -> InstalledPackageInfo
                -> PackageDescription
                -> LocalBuildInfo
                -> Bool
                -> PackageDBStack
                -> IO ()
registerPackage verbosity ipi pd lbi b pkgs = do
  GHC.registerPackage verbosity ipi pd (makeGhcLbi False lbi) b pkgs

componentGhcOptions :: Verbosity
                    -> LocalBuildInfo
                    -> BuildInfo
                    -> ComponentLocalBuildInfo
                    -> FilePath
                    -> GhcOptions
componentGhcOptions verbosity lbi bi clbi odir =
  GHC.componentGhcOptions verbosity (makeGhcLbi False lbi) bi clbi odir
{-
ghcDynamic :: VerbosConfiguredProgram -> IO Bool
ghcDynamic ghcjsProg =
  GHC.ghcDynamic =<< withGhcVersion verbosity ghcjsProg
-}

ghcDynamic :: Compiler -> Bool
ghcDynamic = GHC.ghcDynamic

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

