
import Test.Tasty
import Test.Tasty.HUnit

import ParserTest

main :: IO ()
main = do
  defaultMain (testGroup "Library tests" tests)
    where
      tests = ParserTest.tests


