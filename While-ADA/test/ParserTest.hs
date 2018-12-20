module ParserTest (tests) where
    import WhileParser
    import WhileStructures as WS
    import Test.Tasty
    import Test.Tasty.HUnit

    tests = [testAExp, testBExpr]

    testAExp  = testCase "parse AExp" (assertEqual "" expected result)
        where 
            expected =
                [Sum (Var "x") (Mul (Num 2) (Var "y")),
                 Mul (Var "x") (Mul (Num 2) (Var "y")),
                 Mul (Mul (Var "x") (Num 2)) (Var "y")]
            result =
                [(\[(x,y)]->x) (parse parseAExpr "x+2*y"),
                 (\[(x,y)]->x) (parse parseAExpr "x*(2*y)"),
                 (\[(x,y)]->x) (parse parseAExpr "(x*2)*y")]
    
    testBExpr = testCase "parseBExpr" (assertEqual "" expected result)
        where 
            expected = 
                [LessEq (Min (Num 4) (Num 3)) (Num 5),
                 And (Neg WS.True) (LessEq (Num 8) (Num 2)),
                 And (Neg WS.True) WS.False,
                 Neg (And WS.True WS.False),
                 And (Neg (Eq (Num 4)(Num 8))) (WS.False) 
                ]
            result =
                [(\[(x,y)]->x) (parse parseBExpr "(4-3)<=5"),
                 (\[(x,y)]->x) (parse parseBExpr "!true & (8<=2)"),
                 (\[(x,y)]->x) (parse parseBExpr "!true & false"),
                 (\[(x,y)]->x) (parse parseBExpr "!(true & false)"),
                 (\[(x,y)]->x) (parse parseBExpr "!((4=8)) & false)")
                ]

    
