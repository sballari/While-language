
import Test.Tasty

import Test.Tasty.HUnit

footests = [numericAssign]

main :: IO ()

main = do

  defaultMain (testGroup "Library tests" tests)

    where

      tests = footests


numericAssign = testCase "foo" (assertEqual "" 1 1)