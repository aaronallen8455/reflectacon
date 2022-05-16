{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
module Reflectacon.Class
  ( Reflect(..)
  ) where

import           Data.Kind (Constraint, Type)
import           Data.Type.Equality ((:~:))

class Reflect (kind :: Type) (a :: kind) where
  reflect :: kind

class Unify (a :: k) (b :: k) where
  unify :: Maybe (a :~: b, k)
