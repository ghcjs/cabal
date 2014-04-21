-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.GHC.Base
-- Copyright   :  Isaac Jones 2003-2007
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module contains functions shared by GHC (Distribution.Simple.GHC)
-- and GHC-derived compilers.

{- Copyright (c) 2003-2005, Isaac Jones
All rights reserved.

Redistribution and use in source and binary forms, with or without
modiication, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

module Distribution.Simple.GHC.Base (
        configureToolchain,
        getLanguages,
        getExtensions,
        targetPlatform,
        getGhcInfo
 ) where

import Distribution.Compat.Exception ( catchExit, catchIO )
import Distribution.Simple.Compiler ( Flag )
import Distribution.Simple.Program
         ( Program(..), ConfiguredProgram(..), ProgramConfiguration
         , ProgramLocation(..), ProgramSearchPath, ProgramSearchPathEntry(..)
         , rawSystemProgram, rawSystemProgramStdout, programPath
         , addKnownProgram, arProgram, ldProgram, gccProgram, stripProgram
         , getProgramOutput )
import Distribution.Simple.Program.Types ( suppressOverrideArgs )
import Distribution.Simple.Utils
import Distribution.System ( buildOS, OS(..), Platform, platformFromTriple )
import Distribution.Text ( display, simpleParse )
import Distribution.Verbosity
import Distribution.Version ( Version(..) )
import Language.Haskell.Extension
         ( Language(..), Extension(..), KnownExtension(..) )
import qualified Data.Map as M
import Data.Char ( isSpace )
import Data.Maybe ( maybeToList )
import System.Directory ( getTemporaryDirectory )
import System.FilePath ( (</>), takeDirectory )
import System.IO ( hClose, hPutStrLn )

targetPlatform :: [(String, String)] -> Maybe Platform
targetPlatform ghcInfo = platformFromTriple =<< lookup "Target platform" ghcInfo

-- | Adjust the way we find and configure gcc and ld
--
configureToolchain :: ConfiguredProgram -> M.Map String String
                                        -> ProgramConfiguration
                                        -> ProgramConfiguration
configureToolchain ghcProg ghcInfo =
    addKnownProgram gccProgram {
      programFindLocation = findProg gccProgram extraGccPath,
      programPostConf     = configureGcc
    }
  . addKnownProgram ldProgram {
      programFindLocation = findProg ldProgram extraLdPath,
      programPostConf     = configureLd
    }
  . addKnownProgram arProgram {
      programFindLocation = findProg arProgram extraArPath
    }
  . addKnownProgram stripProgram {
      programFindLocation = findProg stripProgram extraStripPath
    }
  where
    Just ghcVersion = programVersion ghcProg
    compilerDir = takeDirectory (programPath ghcProg)
    baseDir     = takeDirectory compilerDir
    mingwBinDir = baseDir </> "mingw" </> "bin"
    libDir      = baseDir </> "gcc-lib"
    includeDir  = baseDir </> "include" </> "mingw"
    isWindows   = case buildOS of Windows -> True; _ -> False
    binPrefix   = ""

    mkExtraPath :: Maybe FilePath -> FilePath -> [FilePath]
    mkExtraPath mbPath mingwPath | isWindows = mbDir ++ [mingwPath]
                                 | otherwise = mbDir
      where
        mbDir = maybeToList . fmap takeDirectory $ mbPath

    extraGccPath   = mkExtraPath mbGccLocation   windowsExtraGccDir
    extraLdPath    = mkExtraPath mbLdLocation    windowsExtraLdDir
    extraArPath    = mkExtraPath mbArLocation    windowsExtraArDir
    extraStripPath = mkExtraPath mbStripLocation windowsExtraStripDir

    -- on Windows finding and configuring ghc's gcc & binutils is a bit special
    windowsExtraGccDir
      | ghcVersion >= Version [6,12] [] = mingwBinDir </> binPrefix
      | otherwise                       = baseDir
    windowsExtraLdDir
      | ghcVersion >= Version [6,12] [] = mingwBinDir </> binPrefix
      | otherwise                       = libDir
    windowsExtraArDir
      | ghcVersion >= Version [6,12] [] = mingwBinDir </> binPrefix
      | otherwise                       = libDir
    windowsExtraStripDir
      | ghcVersion >= Version [6,12] [] = mingwBinDir </> binPrefix
      | otherwise                       = libDir

    findProg :: Program -> [FilePath]
             -> Verbosity -> ProgramSearchPath -> IO (Maybe FilePath)
    findProg prog extraPath v searchpath =
        programFindLocation prog v searchpath'
      where
        searchpath' = (map ProgramSearchPathDir extraPath) ++ searchpath

    -- Read tool locations from the 'ghc --info' output. Useful when
    -- cross-compiling.
    mbGccLocation   = M.lookup "C compiler command" ghcInfo
    mbLdLocation    = M.lookup "ld command" ghcInfo
    mbArLocation    = M.lookup "ar command" ghcInfo
    mbStripLocation = M.lookup "strip command" ghcInfo

    ccFlags        = getFlags "C compiler flags"
    gccLinkerFlags = getFlags "Gcc Linker flags"
    ldLinkerFlags  = getFlags "Ld Linker flags"

    getFlags key = case M.lookup key ghcInfo of
                   Nothing -> []
                   Just flags ->
                       case reads flags of
                       [(args, "")] -> args
                       _ -> [] -- XXX Should should be an error really

    configureGcc :: Verbosity -> ConfiguredProgram -> IO ConfiguredProgram
    configureGcc v gccProg = do
      gccProg' <- configureGcc' v gccProg
      return gccProg' {
        programDefaultArgs = programDefaultArgs gccProg'
                             ++ ccFlags ++ gccLinkerFlags
      }

    configureGcc' :: Verbosity -> ConfiguredProgram -> IO ConfiguredProgram
    configureGcc'
      | isWindows = \_ gccProg -> case programLocation gccProg of
          -- if it's found on system then it means we're using the result
          -- of programFindLocation above rather than a user-supplied path
          -- Pre GHC 6.12, that meant we should add these flags to tell
          -- ghc's gcc where it lives and thus where gcc can find its
          -- various files:
          FoundOnSystem {}
           | ghcVersion < Version [6,11] [] ->
               return gccProg { programDefaultArgs = ["-B" ++ libDir,
                                                      "-I" ++ includeDir] }
          _ -> return gccProg
      | otherwise = \_ gccProg -> return gccProg

    configureLd :: Verbosity -> ConfiguredProgram -> IO ConfiguredProgram
    configureLd v ldProg = do
      ldProg' <- configureLd' v ldProg
      return ldProg' {
        programDefaultArgs = programDefaultArgs ldProg' ++ ldLinkerFlags
      }

    -- we need to find out if ld supports the -x flag
    configureLd' :: Verbosity -> ConfiguredProgram -> IO ConfiguredProgram
    configureLd' verbosity ldProg = do
      tempDir <- getTemporaryDirectory
      ldx <- withTempFile tempDir ".c" $ \testcfile testchnd ->
             withTempFile tempDir ".o" $ \testofile testohnd -> do
               hPutStrLn testchnd "int foo() { return 0; }"
               hClose testchnd; hClose testohnd
               rawSystemProgram verbosity ghcProg ["-c", testcfile,
                                                   "-o", testofile]
               withTempFile tempDir ".o" $ \testofile' testohnd' ->
                 do
                   hClose testohnd'
                   _ <- rawSystemProgramStdout verbosity ldProg
                     ["-x", "-r", testofile, "-o", testofile']
                   return True
                 `catchIO`   (\_ -> return False)
                 `catchExit` (\_ -> return False)
      if ldx
        then return ldProg { programDefaultArgs = ["-x"] }
        else return ldProg

getLanguages :: Verbosity -> ConfiguredProgram -> IO [(Language, Flag)]
getLanguages _ ghcProg
  -- TODO: should be using --supported-languages rather than hard coding
  | ghcVersion >= Version [7] [] = return [(Haskell98,   "-XHaskell98")
                                          ,(Haskell2010, "-XHaskell2010")]
  | otherwise                    = return [(Haskell98,   "")]
  where
    Just ghcVersion = programVersion ghcProg

getGhcInfo :: Verbosity -> ConfiguredProgram -> IO [(String, String)]
getGhcInfo verbosity ghcProg =
    case programVersion ghcProg of
    Just ghcVersion
     | ghcVersion >= Version [6,7] [] ->
        do xs <- getProgramOutput verbosity (suppressOverrideArgs ghcProg)
                 ["--info"]
           case reads xs of
               [(i, ss)]
                | all isSpace ss ->
                   return i
               _ ->
                   die "Can't parse --info output of GHC"
    _ ->
        return []

getExtensions :: Verbosity -> ConfiguredProgram -> IO [(Extension, Flag)]
getExtensions verbosity ghcProg
  | ghcVersion >= Version [6,7] [] = do

    str <- getProgramOutput verbosity (suppressOverrideArgs ghcProg)
              ["--supported-languages"]
    let extStrs = if ghcVersion >= Version [7] []
                  then lines str
                  else -- Older GHCs only gave us either Foo or NoFoo,
                       -- so we have to work out the other one ourselves
                       [ extStr''
                       | extStr <- lines str
                       , let extStr' = case extStr of
                                       'N' : 'o' : xs -> xs
                                       _              -> "No" ++ extStr
                       , extStr'' <- [extStr, extStr']
                       ]
    let extensions0 = [ (ext, "-X" ++ display ext)
                      | Just ext <- map simpleParse extStrs ]
        extensions1 = if ghcVersion >= Version [6,8]  [] &&
                         ghcVersion <  Version [6,10] []
                      then -- ghc-6.8 introduced RecordPuns however it
                           -- should have been NamedFieldPuns. We now
                           -- encourage packages to use NamedFieldPuns
                           -- so for compatibility we fake support for
                           -- it in ghc-6.8 by making it an alias for
                           -- the old RecordPuns extension.
                           (EnableExtension  NamedFieldPuns, "-XRecordPuns") :
                           (DisableExtension NamedFieldPuns, "-XNoRecordPuns") :
                           extensions0
                      else extensions0
        extensions2 = if ghcVersion <  Version [7,1] []
                      then -- ghc-7.2 split NondecreasingIndentation off
                           -- into a proper extension. Before that it
                           -- was always on.
                           (EnableExtension  NondecreasingIndentation, "") :
                           (DisableExtension NondecreasingIndentation, "") :
                           extensions1
                      else extensions1
    return extensions2

  | otherwise = return oldLanguageExtensions

  where
    Just ghcVersion = programVersion ghcProg

-- | For GHC 6.6.x and earlier, the mapping from supported extensions to flags
oldLanguageExtensions :: [(Extension, Flag)]
oldLanguageExtensions =
    let doFlag (f, (enable, disable)) = [(EnableExtension  f, enable),
                                         (DisableExtension f, disable)]
        fglasgowExts = ("-fglasgow-exts",
                        "") -- This is wrong, but we don't want to turn
                            -- all the extensions off when asked to just
                            -- turn one off
        fFlag flag = ("-f" ++ flag, "-fno-" ++ flag)
    in concatMap doFlag
    [(OverlappingInstances       , fFlag "allow-overlapping-instances")
    ,(TypeSynonymInstances       , fglasgowExts)
    ,(TemplateHaskell            , fFlag "th")
    ,(ForeignFunctionInterface   , fFlag "ffi")
    ,(MonomorphismRestriction    , fFlag "monomorphism-restriction")
    ,(MonoPatBinds               , fFlag "mono-pat-binds")
    ,(UndecidableInstances       , fFlag "allow-undecidable-instances")
    ,(IncoherentInstances        , fFlag "allow-incoherent-instances")
    ,(Arrows                     , fFlag "arrows")
    ,(Generics                   , fFlag "generics")
    ,(ImplicitPrelude            , fFlag "implicit-prelude")
    ,(ImplicitParams             , fFlag "implicit-params")
    ,(CPP                        , ("-cpp", ""{- Wrong -}))
    ,(BangPatterns               , fFlag "bang-patterns")
    ,(KindSignatures             , fglasgowExts)
    ,(RecursiveDo                , fglasgowExts)
    ,(ParallelListComp           , fglasgowExts)
    ,(MultiParamTypeClasses      , fglasgowExts)
    ,(FunctionalDependencies     , fglasgowExts)
    ,(Rank2Types                 , fglasgowExts)
    ,(RankNTypes                 , fglasgowExts)
    ,(PolymorphicComponents      , fglasgowExts)
    ,(ExistentialQuantification  , fglasgowExts)
    ,(ScopedTypeVariables        , fFlag "scoped-type-variables")
    ,(FlexibleContexts           , fglasgowExts)
    ,(FlexibleInstances          , fglasgowExts)
    ,(EmptyDataDecls             , fglasgowExts)
    ,(PatternGuards              , fglasgowExts)
    ,(GeneralizedNewtypeDeriving , fglasgowExts)
    ,(MagicHash                  , fglasgowExts)
    ,(UnicodeSyntax              , fglasgowExts)
    ,(PatternSignatures          , fglasgowExts)
    ,(UnliftedFFITypes           , fglasgowExts)
    ,(LiberalTypeSynonyms        , fglasgowExts)
    ,(TypeOperators              , fglasgowExts)
    ,(GADTs                      , fglasgowExts)
    ,(RelaxedPolyRec             , fglasgowExts)
    ,(ExtendedDefaultRules       , fFlag "extended-default-rules")
    ,(UnboxedTuples              , fglasgowExts)
    ,(DeriveDataTypeable         , fglasgowExts)
    ,(ConstrainedClassMethods    , fglasgowExts)
    ]

