module Reflectacon
  ( plugin
  ) where

import           Control.Applicative (asum)
import           Control.Monad (guard)
import           Data.Foldable
import qualified Data.Generics as Syb
import           Data.Maybe
import           Data.Traversable
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

findClass :: Ghc.Name -> [Ghc.Ct] -> [Ghc.Ct]
findClass name = filter go
  where
    go ct@Ghc.CDictCan{Ghc.cc_class = cls}
      = Ghc.getName cls == name
    go _ = False

explore :: Ghc.DataCon -> Ghc.DataCon
explore dc = trace (Ghc.showSDocUnsafe $ Ghc.ppr (Ghc.dataConOrigArgTys dc)) dc

transform :: Ghc.TyCon -> Ghc.TyCon
transform tc = trace "transform" $!
  case Ghc.isPromotedDataCon_maybe tc of
    Nothing -> tc
    Just dataCon ->
      -- mkTupleTyCon
      Ghc.mkAlgTyCon
        (Ghc.tyConName tc)
        (Ghc.tyConBinders tc)
        (Ghc.tyConResKind tc)
        (Ghc.tyConRoles tc)
        Nothing
        []
        (Ghc.TupleTyCon dataCon Ghc.BoxedTuple)
        (Ghc.VanillaAlgTyCon (Ghc.tyConName tc))
        False

reflectCon :: Ghc.DataCon -> [Ghc.KindOrType] -> Maybe Ghc.CoreExpr
reflectCon con args = do
  argExprs <- for args $ \arg -> do
    Ghc.TyConApp argCon argArgs <- Just arg
    case Ghc.isPromotedDataCon_maybe argCon of
      Nothing -> Just $ Ghc.Type arg
      Just argDataCon -> reflectCon argDataCon argArgs
  Just $ Ghc.mkCoreConApps con argExprs

solver :: Ghc.Name {- -> Ghc.EvBindsVar -} -> Ghc.TcPluginSolver
solver className _ givens wanteds =
  let cts = findClass className wanteds
      solve ct = do -- Maybe
        [_, Ghc.TyConApp con args] <- pure $ Ghc.cc_tyargs ct
        dataCon <- Ghc.isPromotedDataCon_maybe con
        -- guard . null $ Ghc.dataConOrigArgTys dataCon -- only allow enum types
        -- trying to reflect constructors with arguments results in a seg fault.

-- Is it possible to build a core expr that has the datacon applied to 'reflect'
-- instantiated at the right kind and type and then emit new wanted constraints
-- so that in instance is manually constructed just like how the hand written
-- list instance is done?
-- Is the DataCon retrieved from the tyargs already applied to its args?
-- Useful stuff in GHC.Core.Make like mkCoreConApps
        let test = Ghc.mkListExpr Ghc.intTy [Ghc.mkUncheckedIntExpr 3]
        expr <- reflectCon dataCon args
            -- tryThis = Ghc.mkCoreConApps dataCon (applyReflect <$> Ghc.dataConRepArgTys dataCon)
            -- or dataConOrigArgTys might be better
            -- and then form new wanted constraints out of these ^
        let ev = Ghc.EvExpr expr -- . (\x -> trace (Ghc.showSDocUnsafe $ Ghc.ppr test2) x) . Ghc.Var $ Ghc.dataConWorkId dataCon
--               Ghc.EvExpr . Ghc.Var -- . flip Ghc.setVarUnique (Ghc.tyConUnique con)
--                $ Ghc.mkGlobalVar
--                    (Ghc.DataConWorkId dataCon)
--                    (Ghc.tyConName con)
--                    ((\x -> trace (Ghc.showSDocUnsafe $ Ghc.ppr $ Ghc.dataConRepArgTys dataCon) x) $ Ghc.tyConResKind con)
--                    Ghc.vanillaIdInfo
        pure (ev, ct)

   in pure $ Ghc.TcPluginOk (mapMaybe solve cts) []
