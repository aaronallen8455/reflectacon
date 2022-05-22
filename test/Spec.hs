{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
import           Test.HUnit

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
  , "Promoted" ~: MkFoo True False ~=? reflect @_ @('MkFoo 'True 'False)
  , "Syn" ~: "Syn" ~=? reflect @_ @Syn
  , "Nested Syn" ~: Just "Syn" ~=? reflect @_ @(Just Syn)
  ]

data Foo a b = MkFoo a b deriving (Show, Eq)

type Syn = "Syn"
