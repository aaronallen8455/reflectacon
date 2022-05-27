# Reflectacon

This is a GHC plugin that allows for easily reflecting type level promoted data
constructors to the value level. It supplants certain use cases of singletons
and `Known*` classes. It can handle GHC's native type level literals as well as
any type resulting from the promotion of a data constructor via the `DataKinds`
compiler extension.

This is done using the `reflect` method of the `Reflectable` class, which the
plugin generates instances for on the fly.

Example:
```haskell
{-# OPTIONS_GHC -fplugin Reflectacon #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

import Reflectacon (reflect)

data Foo a = MkFoo a deriving Show

foo :: Foo (Maybe Bool)
foo = reflect @_ @('MkFoo ('Just 'True))

nats :: [Integer]
nats = reflect @_ @'[1, 2, 3]

strings :: [String]
strings = reflect @_ @'["these", "are", "symbols"]
```

Currently only GHC 9.0.* and 9.2.* are supported
