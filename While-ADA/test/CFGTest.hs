module CFGTest (tests) where
    import CFG
    import WhileStructures as WS
    import AbsEval
    import Test.Tasty
    import Test.Tasty.HUnit

    tests = [t1,t2,t3,lab1,lab2]

    t1  = testCase "[CFG] assign" (assertEqual "" expected result)
        where 
            expected =  [(L 1, show (Assign "V" (Num 3)),L 2)]   
            result = fst (app (debugCFG (Assign "V" (Num 3))) 1)

    program1 :: Stm
    program1 = Comp (Assign "r" (Var "q")) (While (MoreEq (Var "r") (Var "b")) (Comp (Assign "r" (Sum (Var "r") (Minus (Var "b")))) (Assign "q" (Sum (Var "q") (Num 1)))))

    t2  = testCase "[CFG] book ex" (assertEqual "" expected result)
        where 
            expected =  [(L 1,"Assign \"r\" (Var \"q\")",L 2),
                         (L 2,"Skip",L 3),
                         (L 3,"MoreEq (Var \"r\") (Var \"b\")",L 4),
                         (L 3,"Neg (MoreEq (Var \"r\") (Var \"b\"))",L 7),
                         (L 6,"Skip",L 3),
                         (L 4,"Assign \"r\" (Sum (Var \"r\") (Minus (Var \"b\")))",L 5),
                         (L 5,"Assign \"q\" (Sum (Var \"q\") (Num 1))",L 6)]
            result = fst (app (debugCFG program1) 1)
    
    t3  = testCase "[CFG] book ex + skip finale" (assertEqual "" expected result)
        where 
            expected =  [   (L 1,"Assign \"r\" (Var \"q\")",L 2),
                            (L 2,"Skip",L 3),
                            (L 3,"MoreEq (Var \"r\") (Var \"b\")",L 4),
                            (L 3,"Neg (MoreEq (Var \"r\") (Var \"b\"))",L 7),
                            (L 6,"Skip",L 3),(L 4,"Assign \"r\" (Sum (Var \"r\") (Minus (Var \"b\")))",L 5),(L 5,"Assign \"q\" (Sum (Var \"q\") (Num 1))",L 6),(L 7,"Skip",L 8)]
            result = fst (app (debugCFG (Comp program1 Skip)) 1)

    
    lab1 = testCase "[labOut] x = 3+3" (assertEqual "" expected result)
        where
            expected = ["[L 1] X := 3 + 3"]
            result = fst (app (labelledCode (Assign "X" (Sum (Num 3) (Num 3)))) 1)


    lab2 = testCase "[labOut] ciao" (assertEqual "" expected result)
            where 
                expected = []
                result = fst (app (labelledCode program1) 1)
            
