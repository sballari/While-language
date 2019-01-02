module ParserTest (tests) where
    import WhileParser
    import WhileStructures as WS
    import Test.Tasty
    import Test.Tasty.HUnit

    tests = [testAExp, testBExpr, testSComp, testMComp, testStm, composedIf, composedIf2, oneStm]

    resultP :: Parser a -> String -> a
    resultP p i = (\[(x,y)]->x) (parse p i)

    testAExp  = testCase "parse AExp" (assertEqual "" expected result)
        where 
            expected =
                [Sum (Var "x") (Mul (Num 2) (Var "y")),
                 Mul (Var "x") (Mul (Num 2) (Var "y")),
                 Mul (Mul (Var "x") (Num 2)) (Var "y")]
            result =
                [resultP parseAExpr "x+2*y",
                 resultP parseAExpr "x*(2*y)",
                 resultP parseAExpr "(x*2)*y"]
    
    testBExpr = testCase "parse BExpr" (assertEqual "" expected result)
        where 
            expected = 
                [LessEq (Sum (Num 4) (Minus(Num 3))) (Num 5),
                 And (Neg WS.True) (LessEq (Num 8) (Num 2)),
                 And (Neg WS.True) WS.False,
                 Neg (And WS.True WS.False),
                 And (Neg (Eq (Num 4)(Num 8))) (WS.False) 
                ]
            result =
                [resultP parseBExpr "(4+(-3))<=5",
                 resultP parseBExpr "!true & (8<=2)",
                 resultP parseBExpr "!true & false",
                 resultP parseBExpr "!(true & false)",
                 resultP parseBExpr "!((4=8)) & false)"
                ]

    testStm = testCase "parse a Stm" (assertEqual "" expected result)
        where 
            expected = 
                [Skip,
                 Assign "x" (Num 93),
                 Cond (LessEq (Num 3) (Num 33)) Skip (Assign "x" (Num 1)),
                 While WS.True Skip,
                 Skip]
            result =
                [resultP parseStm "skip",
                 resultP parseStm "x:=93",
                 resultP parseStm "if 3<=33 then skip else x:=1",
                 resultP parseStm "while true do skip",
                 resultP parseStm "(skip)"]

    testSComp = testCase "parse simple composition" (assertEqual "" expected result)
        where 
            expected = [Comp Skip Skip]
            result = [resultP parseStms "skip; skip"]

    testMComp = testCase "parse multiple composition" (assertEqual "" expected result)
        where 
            expected = [Comp Skip (Comp Skip Skip)]
            result = [resultP parseStms "skip; skip; skip"]

    composedIf = testCase "[if] composed if" (assertEqual "" expected result)
        where 
            expected = Comp (Cond (Eq (Num 1) (Num 1)) Skip (Assign "x" (Num 1))) (Assign "y" (Num 2))
            result = resultP parseStms "if 1=1 then skip else x:=1;y:=2"
    
    
    
    composedIf2 = testCase "[if] composed if 2" (assertEqual "" expected result)
        where 
            result = resultP parseStms "if 1=1 then (x:=1;y:=2) else (x:=1;y:=2)"
            expected = Cond (Eq (Num 1) (Num 1)) (Comp (Assign "x" (Num 1)) (Assign "y" (Num 2))) (Comp (Assign "x" (Num 1)) (Assign "y" (Num 2)))

    oneStm = testCase "x:=3" (assertEqual "" expected result)
        where 
            result = resultP parseStms "x:=3"
            expected = Assign "x" (Num 3)
