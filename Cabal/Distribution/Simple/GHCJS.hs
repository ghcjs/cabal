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
        ghcDynamic
  ) where

import Control.Monad (when)
import Distribution.PackageDescription as PD
         ( PackageDescription, BuildInfo(..), Executable(..)
         , Library(..), libModules )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), CompilerId(..), Compiler(..)
         , PackageDBStack, PackageDB(..), Flag )
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(..), ComponentLocalBuildInfo(..) )
import Distribution.Simple.InstallDirs hiding ( absoluteInstallDirs )
import Distribution.Simple.PackageIndex (PackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.Program
         ( Program(..), ConfiguredProgram(..), ProgramConfiguration
         , requireProgramVersion, rawSystemProgramStdoutConf
         , rawSystemProgramStdout
         , userMaybeSpecifyPath, programPath, addKnownProgram
         , ghcjsProgram, ghcjsPkgProgram, hsc2hsProgram
         , ghcProgram, lookupProgram, requireProgram
         )
import Distribution.Text
         ( display, simpleParse )
import Language.Haskell.Extension ( Language(..), Extension(..) )
import Distribution.Verbosity
import Distribution.Version
         ( Version(..), anyVersion, orLaterVersion )
import Distribution.Simple.BuildPaths
import Distribution.Simple.Utils
import Distribution.System ( Platform )
import Data.Monoid ( mconcat )
import System.FilePath          ( (</>), (<.>) )
import qualified Distribution.Simple.Program.HcPkg as HcPkg
import qualified Distribution.Simple.GHC as GHC
import Data.Char ( isSpace )

configure :: Verbosity -> Maybe FilePath -> Maybe FilePath
          -> ProgramConfiguration -> IO (Compiler, Maybe Platform, ProgramConfiguration)
configure verbosity hcPath hcPkgPath conf0 = do
  (ghcjsProg, ghcjsVersion, conf1) <-
    requireProgramVersion verbosity ghcjsProgram
      (orLaterVersion (Version [0,1] []))
      (userMaybeSpecifyPath "ghcjs" hcPath conf0)

  ghcjsGhcVersion <- findProgramVersion "--numeric-ghc-version" id verbosity
    (programPath ghcjsProg)

  -- we also need GHC to be installed
  (ghcProg, ghcVersion, conf2) <-
    requireProgramVersion verbosity ghcProgram
      (orLaterVersion (Version [6,4] []))
      (userMaybeSpecifyPath "ghc" hcPath conf1)

  -- This is slightly tricky, we have to configure ghc first, then we use the
  -- location of ghc to help find ghc-pkg in the case that the user did not
  -- specify the location of ghc-pkg directly:
  (ghcjsPkgProg, ghcjsPkgVersion, conf3) <-
    requireProgramVersion verbosity ghcjsPkgProgram {
      programFindLocation = guessGhcjsPkgFromGhcPath ghcjsProg
    }
    anyVersion (userMaybeSpecifyPath "ghcjs-pkg" hcPkgPath conf2)

  when (ghcjsVersion /= ghcjsPkgVersion) $ die $
       "Version mismatch between ghcjs and ghcjs-pkg: "
    ++ programPath ghcjsProg ++ " is version " ++ display ghcjsVersion ++ " "
    ++ programPath ghcjsPkgProg ++ " is version " ++ display ghcjsPkgVersion

  when (ghcjsGhcVersion /= Just ghcVersion) $ die $
       "Version mismatch between ghcjs and ghc: "
    ++ programPath ghcjsProg ++ " is compiled with ghc version "
    ++ (maybe "<unknown>" display ghcjsGhcVersion) ++ " "
    ++ programPath ghcProg ++ " is version " ++ display ghcVersion

  -- Likewise we try to find the matching hsc2hs program.
  let hsc2hsProgram' = hsc2hsProgram {
                           programFindLocation = GHC.guessHsc2hsFromGhcPath ghcjsProg
                       }
      conf4 = addKnownProgram hsc2hsProgram' conf3

  languages  <- getLanguages verbosity ghcjsProg
  extensions <- getExtensions verbosity ghcjsProg

  ghcInfo <- GHC.getGhcInfo verbosity ghcProg

  let comp = Compiler {
        compilerId             = CompilerId GHCJS ghcjsVersion (Just (CompilerId GHC ghcVersion Nothing)),
        compilerLanguages      = languages,
        compilerExtensions     = extensions
      }
      compPlatform = GHC.targetPlatform ghcInfo
      conf5 = GHC.configureToolchain ghcProg ghcInfo conf4 -- configure gcc and ld
  return (comp, compPlatform, conf5)

getLanguages :: Verbosity -> ConfiguredProgram -> IO [(Language, Flag)]
getLanguages verbosity ghcjsProg =
  -- TODO: should be using --supported-languages rather than hard coding
  return [(Haskell98, "-XHaskell98"), (Haskell2010, "-XHaskell2010")]

getExtensions :: Verbosity -> ConfiguredProgram -> IO [(Extension, Flag)]
getExtensions verbosity ghcjsProg = do
  strs <- fmap lines $ rawSystemStdout verbosity (programPath ghcjsProg) ["--supported-languages"]
  return [ (ext, "-X" ++ display ext) | Just ext <- map simpleParse strs ]

guessGhcjsPkgFromGhcPath :: ConfiguredProgram -> Verbosity -> IO (Maybe FilePath)
guessGhcjsPkgFromGhcPath = GHC.guessToolFromGhcPath "ghcjs-pkg"

-- | Given a single package DB, return all installed packages.
getPackageDBContents :: Verbosity -> PackageDB -> ProgramConfiguration
                        -> IO PackageIndex
getPackageDBContents verbosity packagedb conf = do
  pkgss <- getInstalledPackages' verbosity [packagedb] conf
  toPackageIndex verbosity pkgss conf

getInstalledPackages :: Verbosity -> PackageDBStack -> ProgramConfiguration
                     -> IO PackageIndex
getInstalledPackages verbosity packagedbs conf = do
  GHC.checkPackageDbEnvVar
  GHC.checkPackageDbStack packagedbs
  pkgss <- getInstalledPackages' verbosity packagedbs conf
  topDir <- GHC.ghcLibDir' verbosity ghcjsProg
  let indexes = [ PackageIndex.fromList (map (GHC.substTopDir topDir) pkgs)
                | (_, pkgs) <- pkgss ]
  return $! mconcat indexes
  where
    Just ghcjsProg = lookupProgram ghcjsProgram conf

getInstalledPackages' :: Verbosity -> [PackageDB] -> ProgramConfiguration
                     -> IO [(PackageDB, [InstalledPackageInfo])]
getInstalledPackages' verbosity packagedbs conf =
  sequence
    [ do pkgs <- HcPkg.dump verbosity ghcjsPkgProg packagedb
         return (packagedb, pkgs)
    | packagedb <- packagedbs ]
  where
    Just ghcjsPkgProg = lookupProgram ghcjsPkgProgram conf

-- | Given a list of @(PackageDB, InstalledPackageInfo)@ pairs, produce a
-- @PackageIndex@. Helper function used by 'getPackageDBContents' and
-- 'getInstalledPackages'.
toPackageIndex :: Verbosity
               -> [(PackageDB, [InstalledPackageInfo])]
               -> ProgramConfiguration
               -> IO PackageIndex
toPackageIndex verbosity pkgss conf = do
  -- On Windows, various fields have $topdir/foo rather than full
  -- paths. We need to substitute the right value in so that when
  -- we, for example, call gcc, we have proper paths to give it.
  topDir <- ghcLibDir' verbosity ghcjsProg
  let indices = [ PackageIndex.fromList (map (GHC.substTopDir topDir) pkgs)
                | (_, pkgs) <- pkgss ]
  return $! (mconcat indices)

  where
    Just ghcjsProg = lookupProgram ghcjsProgram conf


buildLib :: Verbosity
            -> PackageDescription
            -> LocalBuildInfo
            -> Library
            -> ComponentLocalBuildInfo
            -> IO ()
buildLib verbosity pkg_descr lbi lib clbi = do
  (ghcProg, _) <- requireProgram verbosity ghcProgram (withPrograms lbi)
  (ghcjsProg, _) <- requireProgram verbosity ghcjsProgram (withPrograms lbi)
  GHC.buildLib' (ghcjsProg `withVersion` ghcProg)
                   False fixOdir libBi verbosity pkg_descr lbi lib clbi
    where
      libBi = libBuildInfo lib
      fixOdir odir _ = odir

buildExe :: Verbosity
            -> PackageDescription
            -> LocalBuildInfo
            -> Executable
            -> ComponentLocalBuildInfo
            -> IO ()
buildExe verbosity pkg_descr lbi exe clbi = do
  (ghcProg, _) <- requireProgram verbosity ghcProgram (withPrograms lbi)
  (ghcjsProg, _) <- requireProgram verbosity ghcjsProgram (withPrograms lbi)
  GHC.buildExe' (ghcjsProg `withVersion` ghcProg) fixOdir exeBi verbosity pkg_descr lbi exe clbi
    where
      exeBi = buildInfo exe
      fixOdir odir _ = odir

installLib    :: Verbosity
              -> LocalBuildInfo
              -> FilePath  -- ^install location
              -> FilePath  -- ^install location for dynamic librariess
              -> FilePath  -- ^Build location
              -> PackageDescription
              -> Library
              -> ComponentLocalBuildInfo
              -> IO ()
installLib verbosity lbi targetDir dynlibTargetDir builtDir pkg lib clbi = do
  GHC.installLib verbosity lbi targetDir dynlibTargetDir builtDir pkg lib clbi
  when (hasLib) $ do copyModuleFiles "js_hi"
                     copyModuleFiles "js"
                     copyModuleFiles "ji"
    where
      hasLib    = not $ null (libModules lib)
                     && null (cSources (libBuildInfo lib))
      copyModuleFiles ext =
        findModuleFiles [builtDir] [ext] (libModules lib)
          >>= installOrdinaryFiles verbosity targetDir

installExe :: Verbosity
              -> LocalBuildInfo
              -> InstallDirs FilePath
              -> FilePath
              -> (FilePath, FilePath)
              -> PackageDescription
              -> Executable
              -> IO ()
installExe verbosity _lbi installDirs buildPref (progprefix, progsuffix) _pkg exe = do
  let binDir = bindir installDirs
  createDirectoryIfMissingVerbose verbosity True binDir
  let exeFileName = exeName exe <.> "jsexe"
      fixedExeBaseName = progprefix ++ exeName exe ++ progsuffix
      installBinary dest = do
          installExecutableFile verbosity
            (buildPref </> exeName exe </> exeFileName)
            (dest <.> exeExtension)
--  installBinary (binDir </> fixedExeBaseName)
  return ()

libAbiHash :: Verbosity
              -> PackageDescription
              -> LocalBuildInfo
              -> Library
              -> ComponentLocalBuildInfo
              -> IO String
libAbiHash verbosity pkg_descr lbi lib clbi =
  GHC.libAbiHash' ghcjsProgram (libBuildInfo lib) verbosity pkg_descr lbi lib clbi

initPackageDB :: Verbosity
                 -> ProgramConfiguration -> FilePath -> IO ()
initPackageDB = GHC.initPackageDB' ghcjsPkgProgram

registerPackage :: Verbosity
                   -> InstalledPackageInfo
                   -> PackageDescription
                   -> LocalBuildInfo
                   -> Bool
                   -> PackageDBStack
                   -> IO ()
registerPackage = GHC.registerPackage' ghcjsPkgProgram

componentGhcOptions :: Verbosity
                       -> LocalBuildInfo
                       -> BuildInfo
                       -> ComponentLocalBuildInfo
                       -> FilePath
                       -> GHC.GhcOptions
componentGhcOptions = GHC.componentGhcOptions

ghcLibDir :: Verbosity -> LocalBuildInfo -> IO FilePath
ghcLibDir verbosity lbi =
    (reverse . dropWhile isSpace . reverse) `fmap`
     rawSystemProgramStdoutConf verbosity ghcjsProgram (withPrograms lbi) ["--print-libdir"]

ghcLibDir' :: Verbosity -> ConfiguredProgram -> IO FilePath
ghcLibDir' verbosity ghcjsProg =
    (reverse . dropWhile isSpace . reverse) `fmap`
     rawSystemProgramStdout verbosity ghcjsProg ["--print-libdir"]

ghcDynamic :: Verbosity -> ConfiguredProgram -> IO Bool
ghcDynamic = GHC.ghcDynamic

withVersion :: ConfiguredProgram -> ConfiguredProgram -> ConfiguredProgram
withVersion ghcjsProg ghcProg = ghcjsProg { programVersion = programVersion ghcProg }
