{-# LANGUAGE LambdaCase #-}
module Reflectacon
  ( Reflectable(..)
  , RewriteLits
  , plugin
  , unify
  ) where

import           Control.Applicative (empty)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe
import           Data.Maybe
import           Data.Traversable

import qualified Reflectacon.GhcFacade as Ghc
import           Reflectacon.Class

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.tcPlugin = const (Just tcPlugin)
  , Ghc.pluginRecompile = Ghc.purePlugin
  }

tcPlugin :: Ghc.TcPlugin
tcPlugin =
  Ghc.TcPlugin
    { Ghc.tcPluginInit = lookupClass "Reflectable"
    , Ghc.tcPluginSolve = solver
    -- , Ghc.tcPluginRewrite = const Ghc.emptyUFM
    , Ghc.tcPluginStop = const $ pure ()
    }

lookupClass :: String -> Ghc.TcPluginM Ghc.Name
lookupClass className = do
  Ghc.Found _ classMod <-
    Ghc.findImportedModule (Ghc.mkModuleName "Reflectacon.Class") Nothing
  Ghc.lookupOrig classMod (Ghc.mkClsOcc className)

matchesClassName :: Ghc.Name -> Ghc.Ct -> Bool
matchesClassName name = \case
  Ghc.CDictCan{Ghc.cc_class = cls} -> Ghc.getName cls == name
  _ -> False

reflectPromotedCon :: Ghc.DataCon -> [Ghc.KindOrType] -> MaybeT Ghc.TcPluginM Ghc.CoreExpr
reflectPromotedCon con args = do
  argExprs <- for args $ \arg -> case arg of
    Ghc.TyConApp argCon argArgs ->
      case Ghc.isPromotedDataCon_maybe argCon of
        Nothing -> pure $ Ghc.Type arg
        Just argDataCon -> reflectPromotedCon argDataCon argArgs
    Ghc.LitTy tyLit -> lift $ reflectTyLit tyLit
    _ -> empty
  pure $ Ghc.mkCoreConApps con argExprs

reflectTyLit :: Ghc.TyLit -> Ghc.TcPluginM Ghc.CoreExpr
reflectTyLit = \case
  Ghc.NumTyLit integer -> pure $ Ghc.mkIntegerExpr integer
  Ghc.StrTyLit string ->
    Ghc.mkStringExprFSWith
      (fmap Ghc.tyThingId . Ghc.tcLookupGlobal)
      string
  Ghc.CharTyLit ch -> pure $ Ghc.mkCharExpr ch

solver :: Ghc.Name {- -> Ghc.EvBindsVar -} -> Ghc.TcPluginSolver
solver className _ _ wanteds = do
  let cts = filter (matchesClassName className) wanteds
      solve ct = do
        [_, ty] <- pure $ Ghc.cc_tyargs ct
        expr <- case ty of
          Ghc.TyConApp con args -> do
            dataCon <- MaybeT . pure $ Ghc.isPromotedDataCon_maybe con
            reflectPromotedCon dataCon args
          Ghc.LitTy tyLit ->
            lift $ reflectTyLit tyLit
          _ -> empty
        pure (Ghc.EvExpr expr, ct)

  solvedCts <- catMaybes <$> traverse (runMaybeT . solve) cts

  pure $ Ghc.TcPluginOk solvedCts []
