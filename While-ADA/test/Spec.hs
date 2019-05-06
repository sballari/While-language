
import Test.Tasty
import Test.Tasty.HUnit

import ParserTest
import SignDomainTest
import AbsStateTest
import CFGTest
import IntervalDomainTest

main :: IO ()
main = do
  defaultMain (testGroup "Library tests" tests)
    where
      tests = ParserTest.tests
              ++ CFGTest.tests 
              ++ SignDomainTest.tests
              ++ IntervalDomainTest.tests
               {- ++ AbsStateTest.tests -}

