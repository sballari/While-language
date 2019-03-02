module CFGTest (tests) where
    import CFG
    import WhileStructures as WS
    import AbsEval
    import Test.Tasty
    import Test.Tasty.HUnit

    tests = [t1]

    t1  = testCase "[CFG] assign" (assertEqual "" expected result)
        where 
            expected =  [(L 1, show (Assign "V" (Num 3)),L 2)]   
            result = fst (app (debugCFG (Assign "V" (Num 3))) 1)
                