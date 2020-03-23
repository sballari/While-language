
import Test.Tasty
import Test.Tasty.HUnit

import ParserTest
import SignDomainTest
import AbsStateTest
import CFGTest
import GJTest
import MinimizePTest
import IntervalDomainTest
import ShowTest
import KarrDomainTest

main :: IO ()
main = do
  defaultMain (testGroup "Library tests" tests)
    where
      tests = 
              ParserTest.tests ++
              CFGTest.tests  ++
              SignDomainTest.tests ++
              IntervalDomainTest.tests ++
              ShowTest.tests ++
              AbsStateTest.tests ++ 
              GJTest.tests ++
              KarrDomainTest.tests ++
              MinimizePTest.tests
                 

