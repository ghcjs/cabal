module Distribution.Simple.Test.Utils
       ( testSuiteCmd
       , testOption
       ) where

import qualified Distribution.Simple.GHCJS as GHCJS
import qualified Distribution.Simple.LocalBuildInfo as LBI
import qualified Distribution.PackageDescription as PD
import Distribution.Simple.BuildPaths ( exeExtension )
import Distribution.Simple.Compiler
    ( Compiler(..), CompilerFlavor(..), compilerFlavor )
import Distribution.Simple.InstallDirs
    ( fromPathTemplate, initialPathTemplateEnv, PathTemplateVariable(..)
    , substPathTemplate , toPathTemplate, PathTemplate )
import Distribution.Simple.Utils ( die )

import Control.Monad ( unless )
import System.FilePath ( (</>), (<.>) )
import System.Directory ( doesFileExist )

testSuiteCmd :: LBI.LocalBuildInfo
             -> FilePath
             -> String
             -> IO (FilePath, [String])
testSuiteCmd lbi name typ = case compilerFlavor (LBI.compiler lbi) of
  GHCJS -> do
    let (testScript, ghcjsPath, ghcjsArgs) =
          GHCJS.ghcjsRunCmd
          (LBI.withPrograms lbi)
          (LBI.buildDir lbi </> name </> name)
    checkExists testScript
    return (ghcjsPath, ghcjsArgs ++ [testScript])
  _     -> do
    let cmd = LBI.buildDir lbi </> name </> name <.> exeExtension
    checkExists cmd
    return (cmd, [])
  where
    checkExists cmd = do
      -- Check that the test program exists.
      exists <- doesFileExist cmd
      unless exists $ die $ "Error: Could not find " ++ typ ++ " program \"" ++
                            cmd ++ "\". Did you build the package first?"

-- TODO: This is abusing the notion of a 'PathTemplate'.  The result
-- isn't neccesarily a path.
testOption :: PD.PackageDescription
           -> LBI.LocalBuildInfo
           -> PD.TestSuite
           -> PathTemplate
           -> String
testOption pkg_descr lbi suite template =
    fromPathTemplate $ substPathTemplate env template
  where
    env = initialPathTemplateEnv
          (PD.package pkg_descr) (LBI.pkgKey lbi)
          (compilerId $ LBI.compiler lbi) (LBI.hostPlatform lbi) ++
          [(TestSuiteNameVar, toPathTemplate $ PD.testName suite)]

