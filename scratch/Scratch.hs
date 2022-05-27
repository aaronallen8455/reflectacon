{-# LANGUAGE BangPatterns #-}
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

test :: forall (b :: Bool). Reflectable b => Bool
test = reflect @_ @b

test2 :: forall k (a :: k). Reflectable a => RewriteLits k
test2 = reflect @_ @a

newtype Foo a = Foo a deriving Show

data Ex where
  MkEx :: a -> Ex
instance Show Ex where show _ = "MkEx"

type family TyFam a where
  TyFam "Just True" = 'Just 'True

x :: Foo (Maybe Bool)
x = reflect @(Foo (Maybe Bool)) @('Foo (TyFam "Just True"))

y :: Ex
y = reflect @_ @('MkEx 'True)

type Syn = '(1, TyFam "Just True", "test")

syn :: Foo (Integer, Maybe Bool, String)
syn = reflect @_ @('Foo Syn)

getSymbols :: forall (symbols :: [Symbol]). Reflectable symbols => [String]
getSymbols = reflect @_ @symbols

--bar :: Bar Bool String
bar = reflect @_ @(MkBar False "test")

data Bar a b = MkBar a b deriving (Show, Eq)

z :: Bool
z = reflect @_ @(MkBar False "test") == MkBar False "test"

data Z = Z [Bool] deriving Show

zz :: Z
zz = reflect @_ @('Z '[True, False])

data HasVar b = MkHasVar (Maybe b) deriving Show
zzzz = reflect @_ @(MkHasVar (Just "test"))

x' :: Bool
x' = reflect @_ @'True

