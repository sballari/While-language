module KarrDomainTest where
    import KarrDomain as KD
    import AbsDomainR
    import WhileStructures
    import Test.Tasty
    import Test.Tasty.HUnit

    tests = [a0,a1,c1,c2,c3]

    a0 = testCase "[KarrDomain Test][a0] join (ex5.5 p109)" (assertEqual "" expected result) 
        where
            expected = EQs ([
                            [1,1,0,0,0,0,0,-3],
                            [0,0,0,0,0,0,1,1],
                            [0,0,1,0,0,0,0,1],
                            [0,0,0,1,0,0,0,10],
                            [0,0,0,0,1,0,0,-2],
                            [0,0,0,0,0,1,0,-12]]
                            ,[11,1,1,10,0,0],o)
            result = join p1 p2
            p1 = EQs ([[1,0],[0,1]],[1,10],o) -- programs s.t. x1 = 1 and x2 = 10
            p2 = EQs ([[1,0],[0,1]],[2,12],o) -- programs s.t. x1 = 2 and x2 = 12
            o = ["x","y"]

    a1 = testCase "[KarrDomain Test][a1] explicit_join (ex5.5 p109)" (assertEqual "" expected result) 
        where
            expected = EQs ([
                [-1,-1,1,1,1,1,0,0],
                [0,0,0,0,0,0,1,1],
                [0,0,1,0,0,0,-1,0],
                [0,0,0,1,0,0,-10,0],
                [0,0,0,0,1,0,0,-2],
                [0,0,0,0,0,1,0,-12]]
                ,[0,1,0,0,0,0],o)
            result = explicit_join p1 p2
            p1 = EQs ([[1,0],[0,1]],[1,10],o) -- programs s.t. x1 = 1 and x2 = 10
            p2 = EQs ([[1,0],[0,1]],[2,12],o) -- programs s.t. x1 = 2 and x2 = 12
            o = ["x","y","z"]
    
    c1 = testCase "[KarrDomain Test][c1] condC x+y = 3x -2" (assertEqual "" expected result) 
        where
            expected = EQs ([[1,0,0],[0,1,0],[0,0,1],[-2,1,0]],[1,10,2,-2],o)
            result = condC bExpr sys
            sys = EQs ([[1,0,0],[0,1,0],[0,0,1]],[1,10,2],o)
            o = ["x","y","z"]
            bExpr =  Eq (Sum (Var "x") (Var "y")) (Sum (Mul (Num 3) (Var "x")) (Minus (Num 2)) )
    c2 = testCase "[KarrDomain Test][c2] condC 2*(x+y) = 3x -2" (assertEqual "" expected result) 
        where
            expected = EQs ([[1,0,0],[0,1,0],[0,0,1],[-1,2,0]],[1,10,2,-2],o)
            result = condC bExpr sys
            sys = EQs ([[1,0,0],[0,1,0],[0,0,1]],[1,10,2],o)
            o = ["x","y","z"]
            bExpr =  Eq (Mul (Num 2) ((Sum (Var "x") (Var "y")))) (Sum (Mul (Num 3) (Var "x")) (Minus (Num 2)) )
    c3 = testCase "[KarrDomain Test][c2] condC z*(x+y) = 3x -2" (assertEqual "" expected result) 
        where
            expected = sys
            result = condC bExpr sys
            sys = EQs ([[1,0,0],[0,1,0],[0,0,1]],[1,10,2],o)
            o = ["x","y","z"]
            bExpr =  Eq (Mul (Var "z")(  (Sum (Var "x") (Var "y")))) (Sum (Mul (Num 3) (Var "x")) (Minus (Num 2)) )