{-# LANGUAGE TypeApplications #-}
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
  ) where

import           Data.Kind (Constraint, Type)
import           GHC.TypeLits
import           Type.Reflection

class Reflectable (kind :: Type) (a :: kind) where
  reflect_ :: RewriteLits kind

-- | Reflect a promoted data constructor or type level iteral to a value.
--
-- >>> reflect @_ @('Just 'True)
-- Just True
--
-- >>> reflect @_ @'[1, 2, 3]
-- [1,2,3]
reflect :: forall kind (a :: kind). Reflectable kind a => RewriteLits kind
reflect = reflect_ @kind @a

type family RewriteLits (ty :: k) :: k where
  RewriteLits Symbol  = String
  RewriteLits Nat     = Integer
  RewriteLits (con a) = (RewriteLits con) (RewriteLits a)
  RewriteLits a       = a
