module SignDomainTest  where
    import SignDomain as SD
    import AbsState as AS
    import AbsDomain as AD
    import WhileParser
    import WhileStructures as WS
    import Test.Tasty
    import Test.Tasty.HUnit
    import AbsEval
    import AbsDenSem
    
    

    tests = [ r1,r2,a1,a2]

    --relazione d'ordine
    r1 = testCase "<=" (assertEqual "" expected result)
        where 
            expected = [True,False, False, False, False, 
                        True, True, False, False, False,
                        True, False, True, False, False,
                        True, True, True, True, False,
                        True, True, True, True, True]
            result = [  (AD.<=) SignTop SignTop,
                        (AD.<=) SignTop LessEqZero,
                        (AD.<=) SignTop MoreEqZero,
                        (AD.<=) SignTop Zero,
                        (AD.<=) SignTop SignBottom,

                        (AD.<=) LessEqZero SignTop,
                        (AD.<=) LessEqZero LessEqZero,
                        (AD.<=) LessEqZero MoreEqZero,
                        (AD.<=) LessEqZero Zero,
                        (AD.<=) LessEqZero SignBottom,

                        (AD.<=) MoreEqZero SignTop,
                        (AD.<=) MoreEqZero LessEqZero,
                        (AD.<=) MoreEqZero MoreEqZero,
                        (AD.<=) MoreEqZero Zero,
                        (AD.<=) MoreEqZero SignBottom,

                        (AD.<=) Zero SignTop,
                        (AD.<=) Zero LessEqZero,
                        (AD.<=) Zero MoreEqZero,
                        (AD.<=) Zero Zero,
                        (AD.<=) Zero SignBottom,

                        (AD.<=) SignBottom SignTop,
                        (AD.<=) SignBottom LessEqZero,
                        (AD.<=) SignBottom MoreEqZero,
                        (AD.<=) SignBottom Zero,
                        (AD.<=) SignBottom SignBottom
                        ]
    
    r2 = testCase "sign cond R>=B" (assertEqual "" expected result)
        where 
            expected = S[("Q",MoreEqZero),("R",MoreEqZero),("B",MoreEqZero)]
            result = signCondC (MoreEq (Var "R") (Var "B")) (S[("Q",MoreEqZero),("R",SignTop),("B",MoreEqZero)])

    a1 = testCase "[sign dom] if R>=B then R=R-B else skip" (assertEqual "" expected result)
        where 
            expected = S[("Q",MoreEqZero),("R",MoreEqZero),("B",SignTop)]
            testprog = Cond (MoreEq (Var "R") (Var "B")) (Assign ("R") (Sum (Var "R") (Minus(Var "B")))) Skip
            initState = S[("Q",MoreEqZero),("R",SignTop),("B",SignTop)]
            result = semS signCondC testprog initState
            
    a2 = testCase "[sign dom] if R<=B then B=B-R else skip" (assertEqual "" expected result)
        where 
            expected = S[("Q",MoreEqZero),("R",MoreEqZero),("B",SignTop)]
            testprog = Cond (LessEq (Var "R") (Var "B")) (Assign ("B") (Sum (Var "B") (Minus(Var "R")))) Skip
            initState = S[("Q",MoreEqZero),("R",SignTop),("B",SignTop)]
            result = semS signCondC testprog initState