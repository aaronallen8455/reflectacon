{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
module Reflectacon.Class
  ( Reflectable
  , reflect
  , RewriteLits
  , unify
  ) where

import           Data.Kind (Constraint, Type)
import           GHC.TypeLits
import           Type.Reflection

class Reflectable (kind :: Type) (a :: kind) where
  reflect_ :: RewriteLits kind

reflect :: forall kind (a :: kind). Reflectable kind a => RewriteLits kind
reflect = reflect_ @kind @a

type family RewriteLits (ty :: k) :: k where
  RewriteLits Symbol  = String
  RewriteLits Nat     = Integer
  RewriteLits (con a) = (RewriteLits con) (RewriteLits a)
  RewriteLits a       = a

-- | Helper that uses 'Typeable' to check type equality
unify :: forall a b. (Typeable a, Typeable b)
      => Maybe (a :~~: b)
unify = eqTypeRep (typeRep @a) (typeRep @b)
