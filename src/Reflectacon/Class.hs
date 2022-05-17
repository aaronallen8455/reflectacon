{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
module Reflectacon.Class
  ( Reflect(..)
  , RewriteLits
  ) where

import           Data.Kind (Constraint, Type)
-- import           Data.Type.Equality ((:~:))
import           Numeric.Natural
import           GHC.TypeLits

class Reflect (kind :: Type) (a :: kind) where
  reflect :: RewriteLits kind

type family RewriteLits (ty :: k) :: k where
  RewriteLits Symbol  = String
  RewriteLits Nat     = Natural
  RewriteLits (con a) = (RewriteLits con) (RewriteLits a)
  RewriteLits a       = a

-- TODO
-- class Unify (k :: Type) (a :: k) (b :: k) where
--   unify :: Maybe (a :~: b)
