{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
module Scratch where

import           Reflectacon.Class
import           Numeric.Natural

test :: forall (b :: Bool). Reflect Bool b => Bool
test = reflect @_ @b

test2 :: forall k (a :: k). Reflect k a => RewriteLits k
test2 = reflect @_ @a

newtype Foo a = Foo a deriving Show

x :: Foo (Maybe [Natural])
x = reflect @_ @('Foo (Just [34, 5]))
