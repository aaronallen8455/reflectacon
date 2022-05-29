{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
module Reflectacon.Class
  ( Reflectable -- method not exported so that instances cannot be defined
  , reflect
  , RewriteLits
  ) where

import           Data.Kind (Constraint, Type)
import           GHC.TypeLits
import           Type.Reflection

-- | Indicates that a type can be reflected.
--
-- Example:
-- @
-- getSymbols :: forall symbols. Reflectable [Symbol] symbols => [String]
-- getSymbols = reflect \@_ \@symbols
-- @
class Reflectable (a :: kind) where
  reflect_ :: RewriteLits kind

-- | Reflect a promoted data constructor or type level iteral to a value.
--
-- >>> reflect @_ @('Just 'True)
-- Just True
--
-- >>> reflect @_ @'[1, 2, 3]
-- [1,2,3]
reflect :: forall kind (a :: kind). Reflectable a => RewriteLits kind
reflect = reflect_ @kind @a
{-# NOINLINE reflect #-}
-- NB: the simplifier can panic without this NOINLINE. TODO figure out why.

type family RewriteLits (ty :: k) :: k where
  RewriteLits Symbol  = String
  RewriteLits Nat     = Integer
  RewriteLits (con a) = (RewriteLits con) (RewriteLits a)
  RewriteLits a       = a
