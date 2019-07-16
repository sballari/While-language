module ParserTest (tests) where
    import WhileParser
    import WhileStructures as WS
    import Test.Tasty
    import Test.Tasty.HUnit

    tests = [variablesT, testAExp, testBExpr, testSComp, testMComp, testStm, composedIf, composedIf2, oneStm,whileP,tb1]

    

    variablesT  = testCase "[variables]" (assertEqual "" expected result)
        where 
            expected = ["x"]
            result = variables program
            program = While (NotEq (Var "x") (Num 0)) (Assign "x" (Sum (Var "x") (Num 1)))

    testAExp  = testCase "parse AExp" (assertEqual "" expected result)
        where 
            expected =
                [Sum (Var "x") (Mul (Num 2) (Var "y")),
                 Mul (Var "x") (Mul (Num 2) (Var "y")),
                 Mul (Mul (Var "x") (Num 2)) (Var "y"),
                 Mul (Div (Var "x") (Num 3)) (Num 2),
                 Div (Var "x") (Mul  (Num 3) (Num 2))]
            result =
                [resultP parseAExpr "x+2*y",
                 resultP parseAExpr "x*(2*y)",
                 resultP parseAExpr "(x*2)*y",
                 resultP parseAExpr "(x/3)*2",
                 resultP parseAExpr "x/3*2"]

                 
    tb1 = testCase "parse x<2 & x<6" (assertEqual "" expected result)
                 where 
                     expected = 
                          And (Less (Var "x") (Num 2)) (Less (Var "x") (Num 6))
                     result =
                         resultP parseBExpr "x<2 & x<6"


    testBExpr = testCase "parse BExpr" (assertEqual "" expected result)
        where 
            expected = 
                [LessEq (Sum (Num 4) (Minus(Num 3))) (Num 5),
                 And (Neg WTrue) (LessEq (Num 8) (Num 2)),
                 And (Neg WTrue) WFalse,
                 Neg (And WTrue WFalse),
                 And (Neg (Eq (Num 4)(Num 8))) (WFalse) 
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
                 While WTrue Skip,
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
    
    whileP = testCase "parse while example" (assertEqual "" expected result)
            where 
                result = resultP parseStms program1
                expected = Comp (Assign "r" (Var "q")) (While (MoreEq (Var "r") (Var "b")) (Comp (Assign "r" (Sum (Var "r") (Minus (Var "b")))) (Assign "q" (Sum (Var "q") (Num 1)))))
        
        
    program1 :: String
    program1 = "r:=q; while r >= b do (r:=r-b;q:=q+1)"
