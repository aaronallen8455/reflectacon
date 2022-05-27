{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
import           Test.HUnit
import           GHC.TypeLits

import           Reflectacon

main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests = test
  [ "Symbol" ~: "test" ~=? reflect @_ @"test"
  , "Natural" ~: 3 ~=? reflect @_ @3
#if MIN_VERSION_ghc(9,2,0)
  , "Char" ~: 'a' ~=? reflect @_ @'a'
#endif
  , "Promoted" ~: MkFoo True True ~=? reflect @_ @('MkFoo 'True 'True)
  , "Syn" ~: "Syn" ~=? reflect @_ @Syn
  , "Nested Syn" ~: Just "Syn" ~=? reflect @_ @(Just Syn)
  , "Syn of syn" ~: "Syn" ~=? reflect @_ @Syn2
  ]

data Foo a b = MkFoo a b deriving (Show, Eq)

type Syn = "Syn"

type Syn2 = Syn

-- A panic with 8.10.x because it tries to get the RuntimeRep of a promoted type.
-- I don't know why this happens and why it only happens in this test suite...
-- Simply going to not support 8.10.x for now.
-- As a further experiment, could have reflect return a Maybe and construct
-- a Nothing core expr in the plugin. See the panic still happens in that case
-- Note that the panic only happens if the data constructor has two or more type arguments.
-- If the panic still happens with the Nothing, then it surely not due to the expr
-- being constructed and is instead truly a bug in GHC and 8.10 should not be supported.
