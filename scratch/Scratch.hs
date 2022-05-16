{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
module Scratch where

import           Reflectacon.Class

test :: forall (b :: Bool). Reflect Bool b => Bool
test = reflect @_ @b

test2 :: forall k (a :: k). Reflect k a => k
test2 = reflect @_ @a

x :: (Bool, Bool, Bool)
x = test2 @_ @'(True, True, False)
