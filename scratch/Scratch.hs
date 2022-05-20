{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
module Scratch where

import           Reflectacon.Class
import           GHC.TypeLits

test :: forall (b :: Bool). Reflectable Bool b => Bool
test = reflect @_ @b

test2 :: forall k (a :: k). Reflectable k a => RewriteLits k
test2 = reflect @_ @a

newtype Foo a = Foo a deriving Show

data Ex where
  MkEx :: a -> Ex
instance Show Ex where show _ = "MkEx"

type family TyFam a where
  TyFam "Just True" = 'Just 'True

x :: Foo (Maybe Bool)
x = reflect @_ @('Foo (TyFam "Just True"))

y :: Ex
y = reflect @_ @('MkEx 'True)

type Syn = '(1,2, "test")

syn :: Foo (Integer, Integer, String)
syn = reflect @_ @('Foo Syn)
