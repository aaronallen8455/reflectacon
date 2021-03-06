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
  , "Field lits" ~: MkFoo "Syn" 3 ~=? reflect @_ @(MkFoo Syn2 3)
  ]

data Foo a b = MkFoo a b deriving (Show, Eq)

type Syn = "Syn"

type Syn2 = Syn
