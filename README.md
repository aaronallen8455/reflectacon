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

### Limitations:
- There's one caveat about reflecting promoted constructors that contain string
  and numeric literals: they must be in polymorphic fields.

  Thus you can have
  ```haskell
  newtype Wrapper a = MkWrapper { unWrap :: a }
  x = reflect @_ @(MkWrapper "hello")
  ```
  but this won't compile
  ```haskell
  newtype Wrapper = MkWrapper { unWrap :: Symbol }
  x = reflect @_ @(MkWrapper "hello")
  ```
  Otherwise there'd have to be a value of type `Symbol`, which is not an
  inhabited type, instead of being able to convert it to `String`. As a side
  note, `Char` type literals do not have this limitation.

- Currently only GHC 8.10.x, 9.0.x, and 9.2.x are supported
