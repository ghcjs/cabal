-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Program.HcPkg
-- Copyright   :  Duncan Coutts 2009, 2013
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module provides an library interface to the @hc-pkg@ program.
-- Currently only GHC, GHCJS and LHC have hc-pkg programs.

module Distribution.Simple.Program.HcPkg (
    hcPkgProgram,

    init,
    invoke,
    register,
    reregister,
    unregister,
    expose,
    hide,
    dump,
    list,

    -- * Program invocations
    initInvocation,
    registerInvocation,
    reregisterInvocation,
    unregisterInvocation,
    exposeInvocation,
    hideInvocation,
    dumpInvocation,
    listInvocation,
  ) where

import Prelude hiding (init)
import Distribution.Compiler ( CompilerFlavor(..) )
import Distribution.Simple.GHC.Props ( GhcImplProps (..) )
import Distribution.Package
         ( PackageId, InstalledPackageId(..) )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo, InstalledPackageInfo_(..)
         , showInstalledPackageInfo
         , emptyInstalledPackageInfo, fieldsInstalledPackageInfo )
import Distribution.ParseUtils
import Distribution.Simple.Compiler
         ( PackageDB(..), PackageDBStack )
import Distribution.Simple.Program.Builtin
         ( ghcPkgProgram, ghcjsPkgProgram, lhcPkgProgram )
import Distribution.Simple.Program.Types
         ( Program, ConfiguredProgram(programId) )
import Distribution.Simple.Program.Run
         ( ProgramInvocation(..), IOEncoding(..), programInvocation
         , runProgramInvocation, getProgramInvocationOutput )
import Distribution.Text
         ( display, simpleParse )
import Distribution.Simple.Utils
         ( die )
import Distribution.Verbosity
         ( Verbosity, deafening, silent )
import Distribution.Compat.Exception
         ( catchExit )

import Data.Char
         ( isSpace )
import Data.Maybe
         ( fromMaybe )
import Data.List
         ( stripPrefix )
import System.FilePath as FilePath
         ( (</>), splitPath, splitDirectories, joinPath, isPathSeparator )
import qualified System.FilePath.Posix as FilePath.Posix

-- | Get the appropriate @hc-pkg@ program for the compiler flavor.
--
hcPkgProgram :: CompilerFlavor -> Program
hcPkgProgram GHCJS = ghcjsPkgProgram
hcPkgProgram LHC   = lhcPkgProgram
hcPkgProgram _     = ghcPkgProgram

-- | Call @hc-pkg@ to initialise a package database at the location {path}.
--
-- > hc-pkg init {path}
--
init :: Verbosity -> GhcImplProps -> ConfiguredProgram -> FilePath -> IO ()
init verbosity props hcPkg path =
  runProgramInvocation verbosity
    (initInvocation props hcPkg verbosity path)

-- | Run @hc-pkg@ using a given package DB stack, directly forwarding the
-- provided command-line arguments to it.
invoke :: Verbosity -> GhcImplProps -> ConfiguredProgram
       -> PackageDBStack -> [String] -> IO ()
invoke verbosity props hcPkg dbStack extraArgs =
  runProgramInvocation verbosity invocation
  where
    args       = packageDbStackOpts props dbStack ++ extraArgs
    invocation = programInvocation hcPkg args

-- | Call @hc-pkg@ to register a package.
--
-- > hc-pkg register {filename | -} [--user | --global | --package-db]
--
register :: Verbosity -> GhcImplProps
         -> ConfiguredProgram -> PackageDBStack
         -> Either FilePath
                   InstalledPackageInfo
         -> IO ()
register verbosity props hcPkg packagedb pkgFile =
  runProgramInvocation verbosity
    (registerInvocation props hcPkg verbosity packagedb pkgFile)


-- | Call @hc-pkg@ to re-register a package.
--
-- > hc-pkg register {filename | -} [--user | --global | --package-db]
--
reregister :: Verbosity -> GhcImplProps
           -> ConfiguredProgram -> PackageDBStack
           -> Either FilePath
                     InstalledPackageInfo
           -> IO ()
reregister verbosity props hcPkg packagedb pkgFile =
  runProgramInvocation verbosity
    (reregisterInvocation props hcPkg verbosity packagedb pkgFile)


-- | Call @hc-pkg@ to unregister a package
--
-- > hc-pkg unregister [pkgid] [--user | --global | --package-db]
--
unregister :: Verbosity -> GhcImplProps -> ConfiguredProgram
           -> PackageDB -> PackageId -> IO ()
unregister verbosity props hcPkg packagedb pkgid =
  runProgramInvocation verbosity
    (unregisterInvocation props hcPkg verbosity packagedb pkgid)


-- | Call @hc-pkg@ to expose a package.
--
-- > hc-pkg expose [pkgid] [--user | --global | --package-db]
--
expose :: Verbosity -> GhcImplProps -> ConfiguredProgram
       -> PackageDB -> PackageId -> IO ()
expose verbosity props hcPkg packagedb pkgid =
  runProgramInvocation verbosity
    (exposeInvocation props hcPkg verbosity packagedb pkgid)


-- | Call @hc-pkg@ to hide a package.
--
-- > hc-pkg hide [pkgid] [--user | --global | --package-db]
--
hide :: Verbosity -> GhcImplProps -> ConfiguredProgram
     -> PackageDB -> PackageId -> IO ()
hide verbosity props hcPkg packagedb pkgid =
  runProgramInvocation verbosity
    (hideInvocation props hcPkg verbosity packagedb pkgid)


-- | Call @hc-pkg@ to get all the details of all the packages in the given
-- package database.
--
dump :: Verbosity -> GhcImplProps -> ConfiguredProgram
     -> PackageDB -> IO [InstalledPackageInfo]
dump verbosity props hcPkg packagedb = do

  output <- getProgramInvocationOutput verbosity
              (dumpInvocation props hcPkg verbosity packagedb)
    `catchExit` \_ -> die $ programId hcPkg ++ " dump failed"

  case parsePackages output of
    Left ok -> return ok
    _       -> die $ "failed to parse output of '"
                  ++ programId hcPkg ++ " dump'"

  where
    parsePackages str =
      let parsed = map parseInstalledPackageInfo' (splitPkgs str)
       in case [ msg | ParseFailed msg <- parsed ] of
            []   -> Left [   setInstalledPackageId
                           . maybe id mungePackagePaths pkgroot
                           $ pkg
                         | ParseOk _ (pkgroot, pkg) <- parsed ]
            msgs -> Right msgs

    parseInstalledPackageInfo' =
        parseFieldsFlat fields (Nothing, emptyInstalledPackageInfo)
      where
        fields =     liftFieldFst pkgrootField
               : map liftFieldSnd fieldsInstalledPackageInfo

        pkgrootField =
          simpleField "pkgroot"
            showFilePath    parseFilePathQ
            (fromMaybe "")  (\x _ -> Just x)

        liftFieldFst = liftField fst (\x (_x,y) -> (x,y))
        liftFieldSnd = liftField snd (\y (x,_y) -> (x,y))

    --TODO: this could be a lot faster. We're doing normaliseLineEndings twice
    -- and converting back and forth with lines/unlines.
    splitPkgs :: String -> [String]
    splitPkgs = checkEmpty . map unlines . splitWith ("---" ==) . lines
      where
        -- Handle the case of there being no packages at all.
        checkEmpty [s] | all isSpace s = []
        checkEmpty ss                  = ss

        splitWith :: (a -> Bool) -> [a] -> [[a]]
        splitWith p xs = ys : case zs of
                           []   -> []
                           _:ws -> splitWith p ws
          where (ys,zs) = break p xs

mungePackagePaths :: FilePath -> InstalledPackageInfo -> InstalledPackageInfo
-- Perform path/URL variable substitution as per the Cabal ${pkgroot} spec
-- (http://www.haskell.org/pipermail/libraries/2009-May/011772.html)
-- Paths/URLs can be relative to ${pkgroot} or ${pkgrooturl}.
-- The "pkgroot" is the directory containing the package database.
mungePackagePaths pkgroot pkginfo =
    pkginfo {
      importDirs        = mungePaths (importDirs  pkginfo),
      includeDirs       = mungePaths (includeDirs pkginfo),
      libraryDirs       = mungePaths (libraryDirs pkginfo),
      frameworkDirs     = mungePaths (frameworkDirs pkginfo),
      haddockInterfaces = mungePaths (haddockInterfaces pkginfo),
      haddockHTMLs      = mungeUrls  (haddockHTMLs pkginfo)
    }
  where
    mungePaths = map mungePath
    mungeUrls  = map mungeUrl

    mungePath p = case stripVarPrefix "${pkgroot}" p of
      Just p' -> pkgroot </> p'
      Nothing -> p

    mungeUrl p = case stripVarPrefix "${pkgrooturl}" p of
      Just p' -> toUrlPath pkgroot p'
      Nothing -> p

    toUrlPath r p = "file:///"
                 -- URLs always use posix style '/' separators:
                 ++ FilePath.Posix.joinPath (r : FilePath.splitDirectories p)

    stripVarPrefix var p =
      case splitPath p of
        (root:path') -> case stripPrefix var root of
          Just [sep] | isPathSeparator sep -> Just (joinPath path')
          _                                -> Nothing
        _                                  -> Nothing


-- Older installed package info files did not have the installedPackageId
-- field, so if it is missing then we fill it as the source package ID.
setInstalledPackageId :: InstalledPackageInfo -> InstalledPackageInfo
setInstalledPackageId pkginfo@InstalledPackageInfo {
                        installedPackageId = InstalledPackageId "",
                        sourcePackageId    = pkgid
                      }
                    = pkginfo {
                        --TODO use a proper named function for the conversion
                        -- from source package id to installed package id
                        installedPackageId = InstalledPackageId (display pkgid)
                      }
setInstalledPackageId pkginfo = pkginfo


-- | Call @hc-pkg@ to get the source package Id of all the packages in the
-- given package database.
--
-- This is much less information than with 'dump', but also rather quicker.
-- Note in particular that it does not include the 'InstalledPackageId', just
-- the source 'PackageId' which is not necessarily unique in any package db.
--
list :: Verbosity -> GhcImplProps -> ConfiguredProgram -> PackageDB
     -> IO [PackageId]
list verbosity props hcPkg packagedb = do

  output <- getProgramInvocationOutput verbosity
              (listInvocation props hcPkg verbosity packagedb)
    `catchExit` \_ -> die $ programId hcPkg ++ " list failed"

  case parsePackageIds output of
    Just ok -> return ok
    _       -> die $ "failed to parse output of '"
                  ++ programId hcPkg ++ " list'"

  where
    parsePackageIds = sequence . map simpleParse . words


--------------------------
-- The program invocations
--

initInvocation :: GhcImplProps -> ConfiguredProgram
               -> Verbosity -> FilePath -> ProgramInvocation
initInvocation props hcPkg verbosity path =
    programInvocation hcPkg args
  where
    args = ["init", path]
        ++ verbosityOpts props verbosity

registerInvocation, reregisterInvocation
  :: GhcImplProps -> ConfiguredProgram -> Verbosity -> PackageDBStack
  -> Either FilePath InstalledPackageInfo
  -> ProgramInvocation
registerInvocation   = registerInvocation' "register"
reregisterInvocation = registerInvocation' "update"


registerInvocation' :: String
                    -> GhcImplProps -> ConfiguredProgram -> Verbosity
                    -> PackageDBStack
                    -> Either FilePath InstalledPackageInfo
                    -> ProgramInvocation
registerInvocation' cmdname props hcPkg verbosity packagedbs (Left pkgFile) =
    programInvocation hcPkg args
  where
    args = [cmdname, pkgFile]
        ++ (if noPkgDbStack props
              then [packageDbOpts props (last packagedbs)]
              else packageDbStackOpts props packagedbs)
        ++ verbosityOpts props verbosity

registerInvocation' cmdname props hcPkg verbosity packagedbs (Right pkgInfo) =
    (programInvocation hcPkg args) {
      progInvokeInput         = Just (showInstalledPackageInfo pkgInfo),
      progInvokeInputEncoding = IOEncodingUTF8
    }
  where
    args = [cmdname, "-"]
        ++ (if noPkgDbStack props
              then [packageDbOpts props (last packagedbs)]
              else packageDbStackOpts props packagedbs)
        ++ verbosityOpts props verbosity


unregisterInvocation :: GhcImplProps -> ConfiguredProgram
                     -> Verbosity -> PackageDB -> PackageId
                     -> ProgramInvocation
unregisterInvocation props hcPkg verbosity packagedb pkgid =
  programInvocation hcPkg $
       ["unregister", packageDbOpts props packagedb, display pkgid]
    ++ verbosityOpts props verbosity


exposeInvocation :: GhcImplProps -> ConfiguredProgram
                 -> Verbosity -> PackageDB -> PackageId -> ProgramInvocation
exposeInvocation props hcPkg verbosity packagedb pkgid =
  programInvocation hcPkg $
       ["expose", packageDbOpts props packagedb, display pkgid]
    ++ verbosityOpts props verbosity


hideInvocation :: GhcImplProps -> ConfiguredProgram
               -> Verbosity -> PackageDB -> PackageId -> ProgramInvocation
hideInvocation props hcPkg verbosity packagedb pkgid =
  programInvocation hcPkg $
       ["hide", packageDbOpts props packagedb, display pkgid]
    ++ verbosityOpts props verbosity


dumpInvocation :: GhcImplProps -> ConfiguredProgram
               -> Verbosity -> PackageDB -> ProgramInvocation
dumpInvocation props hcPkg _verbosity packagedb =
    (programInvocation hcPkg args) {
      progInvokeOutputEncoding = IOEncodingUTF8
    }
  where
    args = ["dump", packageDbOpts props packagedb]
        ++ verbosityOpts props silent
           -- We use verbosity level 'silent' because it is important that we
           -- do not contaminate the output with info/debug messages.

listInvocation :: GhcImplProps -> ConfiguredProgram
               -> Verbosity -> PackageDB -> ProgramInvocation
listInvocation props hcPkg _verbosity packagedb =
    (programInvocation hcPkg args) {
      progInvokeOutputEncoding = IOEncodingUTF8
    }
  where
    args = ["list", "--simple-output", packageDbOpts props packagedb]
        ++ verbosityOpts props silent
           -- We use verbosity level 'silent' because it is important that we
           -- do not contaminate the output with info/debug messages.


packageDbStackOpts :: GhcImplProps -> PackageDBStack -> [String]
packageDbStackOpts props dbstack = case dbstack of
  (GlobalPackageDB:UserPackageDB:dbs) -> "--global"
                                       : "--user"
                                       : map specific dbs
  (GlobalPackageDB:dbs)               -> "--global"
                                       : ("--no-user-" ++ packageDbFlag props)
                                       : map specific dbs
  _                                   -> ierror
  where
    specific (SpecificPackageDB db) = "--" ++ packageDbFlag props ++ "=" ++ db
    specific _ = ierror
    ierror :: a
    ierror     = error ("internal error: unexpected package db stack: " ++ show dbstack)

packageDbFlag :: GhcImplProps -> String
packageDbFlag props
  | flagPackageConf props
  = "package-conf"
  | otherwise
  = "package-db"

packageDbOpts :: GhcImplProps -> PackageDB -> String
packageDbOpts _ GlobalPackageDB        = "--global"
packageDbOpts _ UserPackageDB          = "--user"
packageDbOpts props (SpecificPackageDB db) = "--" ++ packageDbFlag props ++ "=" ++ db

verbosityOpts :: GhcImplProps -> Verbosity -> [String]
verbosityOpts props v

  -- ghc-pkg < 6.11 does not support -v
  | flagPkgNoVerbose props
                   = []

  | v >= deafening = ["-v2"]
  | v == silent    = ["-v0"]
  | otherwise      = []

