
import Test.Tasty
import Test.Tasty.HUnit

import ParserTest
import SignDomainTest
import AbsStateTest

main :: IO ()
main = do
  defaultMain (testGroup "Library tests" tests)
    where
      tests = ParserTest.tests ++ SignDomainTest.tests ++ AbsStateTest.tests

