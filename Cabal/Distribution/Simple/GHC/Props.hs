-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.GHC.Props
-- Copyright   :  Isaac Jones 2003-2007
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module contains the data structure describing invocation
-- details for a GHC or GHC-derived compiler, such as supported flags
-- and workarounds for bugs.

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

module Distribution.Simple.GHC.Props (
        GhcImplProps(..), getImplProps,
        ghcVersionImplProps, ghcjsVersionImplProps, lhcVersionImplProps
        ) where

import Distribution.Simple.Compiler ( Compiler(..), CompilerId(..)
                                    , CompilerFlavor(..)
                                    , compilerFlavor, compilerVersion )
import Distribution.Version ( Version(..) )

{- |
     Properties, features and quirks of a GHC-based implementation.

     Compiler flavors based on GHC behave similar enough that some of
     the support code for them is shared. Every implementation has its
     own peculiarities, that may or may not be a direct result of the
     underlying GHC version. This record keeps track of these differences.

     All shared code (i.e. everything not in the Distribution.Simple.FLAVOR
     module) should use implementation props rather than version numbers
     to test for supported features.
-}

data GhcImplProps = GhcImplProps
  { hasCcOdirBug         :: Bool -- ^ bug in -odir handling for C compilations.
  , flagInfoLanguages    :: Bool -- ^ --info and --supported-languages flags
  , fakeRecordPuns       :: Bool -- ^ use -XRecordPuns for NamedFieldPuns
  , flagStubdir          :: Bool -- ^ -stubdir flag supported
  , noPkgDbStack         :: Bool -- ^ no package DB stack supported
  , flagOutputDir        :: Bool -- ^ -outputdir flag supported
  , flagPkgNoVerbose     :: Bool -- ^ ghc-pkg does not support verbosity flags
  , noExtInSplitSuffix   :: Bool -- ^ split-obj suffix does not contain p_o ext
  , flagFfiIncludes      :: Bool -- ^ -#include on command line for FFI includes
  , flagBuildingCabalPkg :: Bool -- ^ -fbuilding-cabal-package flag supported
  , flagPackageId        :: Bool -- ^ -package-id / -package flags supported
  , separateGccMingw     :: Bool -- ^ mingw and gcc are in separate directories
  , supportsHaskell2010  :: Bool -- ^ -XHaskell2010 and -XHaskell98 flags
  , reportsNoExt         :: Bool -- ^ --supported-languages gives Ext and NoExt
  , alwaysNondecIndent   :: Bool -- ^ NondecreasingIndentation is always on
  , flagGhciScript       :: Bool -- ^ -ghci-script flag supported
  , flagPackageConf      :: Bool -- ^ use package-conf instead of package-db
  }

getImplProps :: Compiler -> GhcImplProps
getImplProps comp =
  case compilerFlavor comp of
    GHC   -> ghcVersionImplProps (compilerVersion comp)
    LHC   -> lhcVersionImplProps (compilerVersion comp)
    GHCJS -> case [ v | CompilerId GHC v <- compilerCompat comp ] of
              [ghcVer] -> ghcjsVersionImplProps (compilerVersion comp) ghcVer
              _  -> error ("Distribution.Simple.GHC.Props.getImplProps: " ++
                           "could not find GHC version for GHCJS compiler")
    x     -> error ("Distribution.Simple.GHC.Props.getImplProps only works" ++
                    "for GHC-like compilers (GHC, GHCJS, LHC)" ++
                    ", but found " ++ show x)

ghcVersionImplProps :: Version -> GhcImplProps
ghcVersionImplProps (Version v _) = GhcImplProps
  { hasCcOdirBug         = v <  [6,4,1]
  , flagInfoLanguages    = v >= [6,7]
  , fakeRecordPuns       = v >= [6,8] && v < [6,10]
  , flagStubdir          = v >= [6,8]
  , noPkgDbStack         = v <  [6,9]
  , flagOutputDir        = v >= [6,10]
  , flagPkgNoVerbose     = v <  [6,11]
  , noExtInSplitSuffix   = v <  [6,11]
  , flagFfiIncludes      = v <  [6,11]
  , flagBuildingCabalPkg = v >= [6,11]
  , flagPackageId        = v >  [6,11]
  , separateGccMingw     = v <  [6,12]
  , supportsHaskell2010  = v >= [7]
  , reportsNoExt         = v >= [7]
  , alwaysNondecIndent   = v <  [7,1]
  , flagGhciScript       = v >= [7,2]
  , flagPackageConf      = v <  [7,5]
  }

ghcjsVersionImplProps :: Version -> Version -> GhcImplProps
ghcjsVersionImplProps _ghcjsVer _ghcVer = GhcImplProps
  { hasCcOdirBug         = False
  , flagInfoLanguages    = True
  , fakeRecordPuns       = False
  , flagStubdir          = True
  , noPkgDbStack         = False
  , flagOutputDir        = True
  , flagPkgNoVerbose     = False
  , noExtInSplitSuffix   = False
  , flagFfiIncludes      = False
  , flagBuildingCabalPkg = True
  , flagPackageId        = True
  , separateGccMingw     = False
  , supportsHaskell2010  = True
  , reportsNoExt         = True
  , alwaysNondecIndent   = False
  , flagGhciScript       = True
  , flagPackageConf      = False
  }

lhcVersionImplProps :: Version -> GhcImplProps
lhcVersionImplProps = ghcVersionImplProps