{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
module Scratch where

import           Reflectacon.Class

test :: forall (b :: Bool). Reflectable Bool b => Bool
test = reflect @_ @b

test2 :: forall k (a :: k). Reflectable k a => RewriteLits k
test2 = reflect @_ @a

newtype Foo a = Foo a deriving Show

x :: Foo (Maybe [Integer])
x = reflect @_ @('Foo (Just [34, 5]))
