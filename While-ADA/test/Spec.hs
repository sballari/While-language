
import Test.Tasty
import Test.Tasty.HUnit

import ParserTest
import SignDomainTest
import AbsStateTest
import CFGTest

main :: IO ()
main = do
  defaultMain (testGroup "Library tests" tests)
    where
      tests = ParserTest.tests
              ++ CFGTest.tests 
              ++ SignDomainTest.tests {- ++ AbsStateTest.tests -}

