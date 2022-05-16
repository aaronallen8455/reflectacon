{-# LANGUAGE BangPatterns #-}
module Reflectacon
  ( plugin
  ) where

import           Control.Applicative (asum)
import           Data.Foldable
import qualified Reflectacon.GhcFacade as Ghc

import           Debug.Trace

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.tcPlugin = const tcPlugin
  }

tcPlugin :: Maybe Ghc.TcPlugin
tcPlugin = Just
  Ghc.TcPlugin
    { Ghc.tcPluginInit = lookupClass
    , Ghc.tcPluginSolve = solver
    -- , Ghc.tcPluginRewrite = const Ghc.emptyUFM
    , Ghc.tcPluginStop = const $ pure ()
    }

lookupClass :: Ghc.TcPluginM Ghc.Name
lookupClass = do
  Ghc.Found _ classMod <-
    Ghc.findImportedModule (Ghc.mkModuleName "Reflectacon.Class") Nothing
  Ghc.lookupOrig classMod (Ghc.mkClsOcc "Reflect")

findClass :: Ghc.Name -> [Ghc.Ct] -> Maybe Ghc.Ct
findClass name = find go
  where
    go ct@Ghc.CDictCan{Ghc.cc_class = cls}
      = Ghc.getName cls == name
    go _ = False

explore :: Ghc.DataCon -> Ghc.DataCon
explore dc = trace (Ghc.showSDocUnsafe $ Ghc.ppr (Ghc.dataConOrigArgTys dc)) dc

solver :: Ghc.Name {- -> Ghc.EvBindsVar -} -> Ghc.TcPluginSolver
solver className _ givens wanteds =
  case findClass className wanteds of
    Just ct
      | [_, Ghc.TyConApp con _] <- Ghc.cc_tyargs ct
      , Just dataCon <- explore <$> Ghc.isPromotedDataCon_maybe con
      , null $ Ghc.dataConOrigArgTys dataCon
      -> let ev = Ghc.EvExpr . Ghc.Var $
               Ghc.mkGlobalVar
                 (Ghc.DataConWorkId dataCon) -- should be wrapper instead of worker?
                 (Ghc.tyConName con)
                 (Ghc.tyConKind con)
                 Ghc.vanillaIdInfo
          in pure $ Ghc.TcPluginOk [(ev, ct)] []
    Nothing -> pure $ Ghc.TcPluginOk [] []

    _ -> pure $ Ghc.TcPluginOk [] []
