{-# LANGUAGE CPP #-}
module Reflectacon.GhcFacade
  ( module Ghc
  ) where

#if MIN_VERSION_ghc(9,2,0)

import           GHC.Core.Class as Ghc
import           GHC.Core.TyCo.Rep as Ghc
import           GHC.Core.TyCon as Ghc
import           GHC.Plugins as Ghc hiding (TcPlugin)
import           GHC.Tc.Plugin as Ghc
import           GHC.Tc.Types as Ghc
import           GHC.Tc.Types.Constraint as Ghc
import           GHC.Tc.Types.Evidence as Ghc
import           GHC.Types.TyThing as Ghc

#elif MIN_VERSION_ghc(9,0,0)

import           GHC.Core.Class as Ghc
import           GHC.Core.TyCo.Rep as Ghc
import           GHC.Core.TyCon as Ghc
import           GHC.Plugins as Ghc hiding (TcPlugin)
import           GHC.Tc.Plugin as Ghc
import           GHC.Tc.Types as Ghc
import           GHC.Tc.Types.Constraint as Ghc
import           GHC.Tc.Types.Evidence as Ghc

#elif MIN_VERSION_ghc(8,10,0)

import           Constraint as Ghc
import           CoreSyn as Ghc
import           DataCon as Ghc
import           HscTypes as Ghc
import           MkCore as Ghc
import           Module as Ghc
import           Name as Ghc
import           Outputable as Ghc
import           Plugins as Ghc hiding (TcPlugin)
import           TcEvidence as Ghc
import           TcPluginM as Ghc
import           TcRnTypes as Ghc
import           TyCoRep as Ghc
import           TyCon as Ghc
import           Type as Ghc
import           UniqSet as Ghc

#endif
