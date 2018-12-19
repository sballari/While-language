module ParserTest (tests) where
    import WhileParser
    import WhileStructures
    import Test.Tasty
    import Test.Tasty.HUnit

    tests = [testAExp]

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

    
