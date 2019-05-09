module ShowTest (tests) where
    import WhileStructures as WS
    import Test.Tasty
    import Test.Tasty.HUnit


    tests = [a1]

    a1  = testCase "[SHOW1 AEXPR] X+1" (assertEqual "" expected result)
        where 
            result = "X + 1"
            expected = show (Sum (Var "X") (Num 1))