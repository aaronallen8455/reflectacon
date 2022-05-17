{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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

-- instance (Reflect k a, Reflect [k] rest) => Reflect [k] (a ': rest) where
--   reflect = reflect @k @a : reflect @[k] @rest
-- 
-- instance Reflect [k] '[] where
--   reflect = []

-- write instance for tuples?
-- maybe
-- either
-- etc...

class Unify (k :: Type) (a :: k) (b :: k) where
  unify :: Maybe (a :~: b)
