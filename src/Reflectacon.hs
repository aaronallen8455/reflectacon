{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Reflectacon
  ( Reflectable
  , reflect
  , RewriteLits
  , plugin
  ) where

import           Control.Applicative (empty)
import           Control.Monad (guard)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe
import           Data.Bitraversable
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

reflectPromotedCon
  :: Ghc.UniqSet Ghc.Name
  -> Ghc.DataCon
  -> [Ghc.TyCoVar]
  -> [Ghc.KindOrType]
  -> MaybeT Ghc.TcPluginM Ghc.CoreExpr
reflectPromotedCon conVarsSet con conArgs appArgs = do
  guard $ length conArgs == length appArgs
  argExprs <- for (zip conArgs appArgs) $
    \(conArg, appArg) -> case expandTypeSynonym appArg of
      Ghc.TyConApp argCon argAppArgs ->
        case Ghc.isPromotedDataCon_maybe argCon of
          Nothing -> pure $ Ghc.Type appArg
          Just argDataCon -> do
            newConVarsSet <- case expandTypeSynonym $ Ghc.tyVarKind conArg of
              Ghc.TyConApp conArgCon conArgAppArgs -> do
                let conArgVars = Ghc.binderVars $ Ghc.tyConBinders conArgCon
                guard $ length conArgAppArgs == length conArgVars
                let zipped = zip conArgAppArgs conArgVars
                    filtered =
                      filter ((`Ghc.elementOfUniqSet` conVarsSet) . Ghc.getName . fst)
                      $ mapMaybe (bitraverse maybeTypeIsVar pure) zipped
                pure . Ghc.mkUniqSet $ Ghc.getName . snd <$> filtered

              Ghc.TyVarTy _ -> do
                let argConArgs = Ghc.binderVars $ Ghc.tyConBinders argCon
                pure . Ghc.mkUniqSet $ Ghc.getName <$> argConArgs
              _ -> empty

            reflectPromotedCon
              newConVarsSet
              argDataCon
              (Ghc.binderVars (Ghc.tyConBinders argCon))
              argAppArgs

      Ghc.LitTy tyLit
        -- only allow type literals that are type arguments, except for Char
#if MIN_VERSION_ghc(9,2,0)
        | Ghc.CharTyLit _ <- tyLit
        -> lift $ reflectTyLit tyLit
#endif
        | Just conArgVar <- maybeTypeIsVar $ Ghc.tyVarKind conArg
        , Ghc.elementOfUniqSet (Ghc.getName conArgVar) conVarsSet
        -> lift $ reflectTyLit tyLit

      _ -> empty

  pure $ Ghc.mkCoreConApps con argExprs

reflectTyLit :: Ghc.TyLit -> Ghc.TcPluginM Ghc.CoreExpr
reflectTyLit = \case
  Ghc.NumTyLit integer ->
    pure $ Ghc.mkIntegerExpr integer
  Ghc.StrTyLit string ->
    Ghc.mkStringExprFSWith
      (fmap Ghc.tyThingId . Ghc.tcLookupGlobal)
      string
#if MIN_VERSION_ghc(9,2,0)
  Ghc.CharTyLit ch -> pure $ Ghc.mkCharExpr ch
#endif

solver :: Ghc.Name -> Ghc.TcPluginSolver
solver className _ _ wanteds = do
  let cts = filter (matchesClassName className) wanteds
      solve ct = do
        [_, ty] <- pure $ Ghc.cc_tyargs ct
        expr <- case expandTypeSynonym ty of
          Ghc.TyConApp con appArgs -> do
            let conArgs = Ghc.binderVars $ Ghc.tyConBinders con
                conArgSet = Ghc.mkUniqSet $ Ghc.getName <$> conArgs
            dataCon <- MaybeT . pure $ Ghc.isPromotedDataCon_maybe con
            reflectPromotedCon
              conArgSet
              dataCon
              conArgs
              appArgs
          Ghc.LitTy tyLit ->
            lift $ reflectTyLit tyLit
          _ -> empty
        pure (Ghc.EvExpr expr, ct)

  solvedCts <- catMaybes <$> traverse (runMaybeT . solve) cts

  pure $ Ghc.TcPluginOk solvedCts []

maybeTypeIsVar :: Ghc.Type -> Maybe Ghc.Var
maybeTypeIsVar = \case
  Ghc.TyVarTy var -> Just var
  _ -> Nothing

expandTypeSynonym :: Ghc.Type -> Ghc.Type
expandTypeSynonym = fromMaybe <*> Ghc.tcView
