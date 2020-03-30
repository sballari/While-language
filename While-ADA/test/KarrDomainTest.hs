module KarrDomainTest where
    import KarrDomain as KD
    import AbsDomainR
    import WhileStructures
    import PolyUtils
    import Test.Tasty
    import Test.Tasty.HUnit
    import MatrixUtilities
    import WhileParser
    import AbsDenSemR
    import qualified Data.List as L


    a0 = testCase "[KarrDomain Test][a0] join (ex5.5 p109)" (assertEqual "" expected result) 
        where
            expected = EQs ([[-2,1]],[8],o)
            result = join p1 p2
            p1 = EQs ([[1,0],[0,1]],[1,10],o) -- programs s.t. x1 = 1 and x2 = 10
            p2 = EQs ([[1,0],[0,1]],[2,12],o) -- programs s.t. x1 = 2 and x2 = 12
            o = ["x","y"]

    a1 = testCase "[KarrDomain Test][a1] explicit_join (ex5.5 p109)" (assertEqual "" expected result) 
        where
            expected = EQs ([
                [0,0,1,0,0,0,-1,0],
                [0,0,0,1,0,0,-10,0],
                [0,0,0,0,1,0,0,-2],
                [0,0,0,0,0,1,0,-12],
                [-1,0,1,0,1,0,0,0],
                [0,-1,0,1,0,1,0,0],
                [0,0,0,0,0,0,1,1]]
                ,[0,0,0,0,0,0,1],o)
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

    d1 = testCase "[KarrDomain Test][d1] varPos [x,y,z] x" (assertEqual "" expected result) 
        where
            expected = Just  0
            result = varPos sys "x"  
            sys = EQs ([[1,0,0],[0,1,0],[0,0,1]],[1,10,2],o)
            o = ["x","y","z"]


    d2 = testCase "[KarrDomain Test][d2] varPos [x,y,z] y" (assertEqual "" expected result) 
        where
            expected = Just 1
            result = varPos sys "y"  
            sys = EQs ([[1,0,0],[0,1,0],[0,0,1]],[1,10,2],o)
            o = ["x","y","z"]
    d3 = testCase "[KarrDomain Test][d3] varPos [x,y,z] z" (assertEqual "" expected result) 
        where
            expected = Just 2
            result = varPos sys "z"  
            sys = EQs ([[1,0,0],[0,1,0],[0,0,1]],[1,10,2],o)
            o = ["x","y","z"]
    
    d4 = testCase "[KarrDomain Test][d4] varPos [x,y,z] pippo" (assertEqual "" expected result) 
        where
            expected = Nothing
            result = varPos sys "pippo"  
            sys = EQs ([[1,0,0],[0,1,0],[0,0,1]],[1,10,2],o)
            o = ["x","y","z"]

    e1 = testCase "[MatrixUtilities Test][e1] elimination vj" (assertEqual "" expected result) 
        where
            expected = [[1,0,0,-3/2,11/2],[0,0,1,-15,-13]]
            result = log_elimination sys 1 
            sys = [[0,1/3,0,1,1],[1,1/2,0,0,7],[0,5,1,0,2]]

            
    e2 = testCase "[MatrixUtilities Test][e2] two const elimination vj" (assertEqual "" expected result) 
        where
            expected = [1,0,0,-3/2,11/2]
            result = two_row_el a c 1 
            a = [1,1/2,0,0,7]
            c = [0,1/3,0,1,1]

    f1 = testCase "[KarrDomain Test][f1] y<-unbounded" (assertEqual "" expected result) 
            where
                expected = EQs ([[1,0,0,-3/2],[0,0,1,-15]],[11/2,-13],["x","y","z","w"])
                result = assignUnbounded sys "y"
                sys = EQs ([[0,1/3,0,1],[1,1/2,0,0],[0,5,1,0]],[1,7,2],["x","y","z","w"])
                
    f2 = testCase "[KarrDomain Test][f2] x<-unbounded" (assertEqual "" expected result) 
            where
                expected =EQs ([[0,1/3,0,1],[0,5,1,0]],[1,2],["x","y","z","w"])
                result = assignUnbounded sys "x"
                sys = EQs ([[0,1/3,0,1],[1,1/2,0,0],[0,5,1,0]],[1,7,2],["x","y","z","w"])
    
    g1 = testCase "[KarrDomain Test][g1] x<-expr_lin (invertible)" (assertEqual "" expected result) 
            {-
                assignS z<-z+1 {x+z=1 ^ y+2z=0 } = {x+z=-1 ^ y+2z=2 }
            -}
            where
                expected = EQs ([[1,0,1],[0,1,2]],[2,2],["x","y","z"])
                result = assignS (Assign "z" (Sum (Var "z") (Num 1))) sys
                sys = EQs ([[1,0,1],[0,1,2]],[1,0],["x","y","z"])

    g2 = testCase "[KarrDomain Test][g2] x<-expr_lin (invertible)" (assertEqual "" expected result) 
            {-
                assignS z<-z+1 {x+z=1 ^ y+2z=0 } = {x+z=-1 ^ y+2z=2 }
            -}
            where
                expected = EQs ([[1,0,1],[0,1,2]],[2,2],["x","y","z"])
                result = invertible_assign 2 ([0,0,1],-1) sys
                sys = EQs ([[1,0,1],[0,1,2]],[1,0],["x","y","z"])

    g3 = testCase "[KarrDomain Test][g3] coefficienti z=z++" (assertEqual "" expected result) 
            {-
                assignS z<-z+1 {x+z=1 ^ y+2z=0 } = {x+z=-1 ^ y+2z=2 }
            -}
            where
                expected = ([0,0,1],1)
                result = order o l_pol
                Just l_pol = (minimize e)
                e = Sum (Var "z") (Num 1)
                o = ["x","y","z"]
    g4 = testCase "[KarrDomain Test][g4] inv_coef" (assertEqual "" expected result) 
            {-
                assignS z<-z+1 {x+z=1 ^ y+2z=0 } = {x+z=-1 ^ y+2z=2 }
            -}
            where
                expected = ([0,0,1],-1)
                result = inversion_coefficients cb 2 1
                cb = ([0,0,1],1)


    g5 = testCase "[KarrDomain Test][g5] x<-expr_lin (notn-invertible)" (assertEqual "" expected result) 
            {-
                assignS z<-z+1 {x+z=1 ^ y+2z=0 } = {x+z=-1 ^ y+2z=2 }
            -}
            where
                expected = EQs ([[0,1,2],[1,-1,0]],[0,0],["x","y","z"])
                result = assignS (Assign "x" (Var "y")) sys
                sys = EQs ([[1,0,1],[0,1,2]],[1,0],["x","y","z"])
    g6 = testCase "[KarrDomain Test][g6] x<-y (notn-invertible x<-unbAssing)" (assertEqual "" expected result) 
            {-
                assignS x<-y {x+z=1 ^ y+2z=0 } = 
            -}
            where
                expected = EQs ([[0,1,2]],[0],["x","y","z"])
                result = assignUnbounded sys "x"
                sys = EQs ([[1,0,1],[0,1,2]],[1,0],["x","y","z"])
    g7 = testCase "[KarrDomain Test][g7] x<-y (notn-invertible new constr creation)" (assertEqual "" expected result) 
            {-
                assignS x<-y {x+z=1 ^ y+2z=0 } = 
            -}
            where
                expected = Eq (Sum (Var "x") (Minus (Var "y"))) (Num 0)
                result = assign_to_newConstr "x" (Var "y")
    g8 = testCase "[KarrDomain Test][g8] x<-y (notn-invertible filter)" (assertEqual "" expected result) 
            {-
                assignS x<-y {x+z=1 ^ y+2z=0 } = 
            -}
            where
                expected = EQs ([[0,1,2],[1,-1,0]],[0,0],o)
                result = condC new_constr sys2 
                new_constr = Eq (Sum (Var "x") (Minus (Var "y"))) (Num 0)
                sys2 = EQs ([[0,1,2]],[0],o)
                o = ["x","y","z"]


    h1 = testCase "[KarrDomain Test][h1] Step2 (riduzione join)" (assertEqual "" expected result) 
            where
                expected = ([[1,0,0,0,0,0,0,-1],[0,1,0,0,0,0,0,-2]],[1,10])
                result = row_filter exp_join_RE leading 2 --STEP2
                exp_join_RE = 
                    ([
                    [1,0,0,0,0,0,0,-1],
                    [0,1,0,0,0,0,0,-2],
                    [0,0,1,0,0,0,0,1],
                    [0,0,0,1,0,0,0,10],
                    [0,0,0,0,1,0,0,-2],
                    [0,0,0,0,0,1,0,-12],
                    [0,0,0,0,0,0,1,0]]
                    ,[1,10,1,10,0,0,1])
                leading = [(True,0,0),(True,1,1),(True,2,2),(True,3,3),(True,4,4),(True,5,5),(True,6,6),(False,7,-1)]
                o = ["x","y"]


    h2 = testCase "[KarrDomain Test][h2] Step2 (riduzione join)" (assertEqual "" expected result) 
        where
            expected = ([[1,0,0,0,0,0,0,-1],[0,1,0,0,0,0,0,-2]],[1,10])
            result = row_filter exp_join_RE leading 2 --STEP2
            exp_join_RE = 
                ([
                [0,0,1,0,0,0,0,1],
                [0,0,0,0,0,1,0,-12],
                [0,0,0,1,0,0,0,10],
                [1,0,0,0,0,0,0,-1],
                [0,0,0,0,1,0,0,-2],
                [0,1,0,0,0,0,0,-2],
                [0,0,0,0,0,0,1,0]]
                ,[1,10,1,1,0,10,1])
            leading = [(True,0,3),(True,1,5),(True,2,0),(True,3,2),(True,4,4),(True,5,1),(True,6,6),(False,7,-1)]
            o = ["x","y"]


    h3 = testCase "[KarrDomain Test][h3] Step3 (riduzione join)" (assertEqual "" expected result) 
        where
            expected = [[1,0,-1],[0,1,-2]]
            result = in_base_elimination4Join step2_res leading 2
            step2_res = 
                [[1,0,0,0,0,0,0,-1],[0,1,0,0,0,0,0,-2]]
            leading = [(True,0,0),(True,1,1),(True,2,2),(True,3,3),(True,4,4),(True,5,5),(True,6,6),(False,7,-1)]
            o = ["x","y"]


     
    h4 = testCase "[KarrDomain Test][h4] Step4 (riduzione join)" (assertEqual "" expected result) 
            where
                expected = [[8.0,-2.0,1.0,0.0]] -- [[-4,1,-1/2]]
                result = out_base_elimination4join aug_matrix 2
                aug_matrix = [[1,1,0,-1],[10,0,1,-2]]
                

    h5 = testCase "[KarrDomain Test][h5] augmentation step3-4 (riduzione join)" (assertEqual "" expected result) 
            where
                expected = [[1,1,0,-1],[10,0,1,-2]]
                result = transpose ((f_b:(transpose step3_res)))
                step3_res = [[1,0,-1],[0,1,-2]]
                f_b = [1,10]
                
    
    h6 = testCase "[KarrDomain Test][h6] final (riduzione join)" (assertEqual "" expected result) 
            where
                f_result = [[8.0,-2.0,1.0,0.0]] -- [[-4,1,-1/2]]
                final_coeff = transpose ( L.genericTake 2 (tail (transpose f_result)) )
                final_b = head (transpose f_result)
                result = EQs (final_coeff,final_b,["x","y"])
                expected = EQs ([[-2,1]],[8],["x","y"])


    h7 = testCase "[KarrDomain Test][h7] leadingMatrix inof (riduzione join)" (assertEqual "" expected result) 
            where
                result = leading_matrix_info join_system 2
                join_system = 
                    [
                    [0,0,1,0,0,0,0,1],
                    [0,0,0,0,0,1,0,-12],
                    [0,0,0,1,0,0,0,10],
                    [1,0,0,0,0,0,0,-1],
                    [0,0,0,0,1,0,0,-2],
                    [0,1,0,0,0,0,0,-2],
                    [0,0,0,0,0,0,1,0]]
                    
                expected = [(True,0,3),(True,1,5),(True,2,0),(True,3,2),(True,4,4),(True,5,1),(True,6,6),(False,7,-1)]
                

    i1 = testCase "[KarrDomain Test][i1] analisi assign" (assertEqual "" expected result) 
            where
                expected = EQs ([[1,0],[0,1]],[1,0],["I","X"]) 
                result = semS False progTree  (EQs ([],[],["I","X"]))
                [(progTree,_)] = parse parseStms source_code 
                source_code = "I:=1; X := 0;"

    i2 = testCase "[KarrDomain Test][i2] analisi ex5.8 pag 112" (assertEqual "" expected result) 
            where
                expected = EQs ([],[],["I","X"]) 
                result = semS False progTree  (EQs ([[1,0],[0,1]],[1,0],["I","X"]))
                [(progTree,_)] = parse parseStms source_code 
                source_code = "while I <= 1000 do  (I:=0; X:=X+1)"

    i3 = testCase "[KarrDomain Test][i3] while body 1;" (assertEqual "" expected result) 
            where
                expected = EQs ([[1,0],[0,1]],[2,1],["I","X"]) 
                result = semS False progTree  (EQs ([[1,0],[0,1]],[1,0],["I","X"]))
                [(progTree,_)] = parse parseStms source_code 
                source_code = "I:=I+1; X:=X+1;"
    i4 = testCase "[KarrDomain Test][i4] while body 2;" (assertEqual "" expected result) 
            where
                expected = (EQs ([[-1,1]],[-1],["I","X"]) )

                result = semS False progTree  (EQs ([[-1,1]],[-1],["I","X"]) )
                [(progTree,_)] = parse parseStms source_code 
                source_code = "I:=I+1; X:=X+1;"

    i4a = testCase "[KarrDomain Test][i4a] eq join;" (assertEqual "" expected result) 
            where
                expected = (EQs ([[-1,1]],[-1],["I","X"]) )
                result = (EQs ([[-1,1]],[-1],["I","X"]) ) `join` (EQs ([[-1,1]],[-1],["I","X"]) )
                
    i4b = testCase "[KarrDomain Test][i4b] while body 2 join;" (assertEqual "" expected result) 
            where
                expected = (EQs ([[-1,1]],[-1],["I","X"]) )
                result = r `join` (EQs ([[-1,1]],[-1],["I","X"]) )
                r = (EQs ([[1,0],[0,1]],[1,0],["I","X"]))

    i4c = testCase "[KarrDomain Test][i4c] while body 2 ex_join;" (assertEqual "" expected result) 
            where
                expected = (EQs ([[-1,1]],[-1],["I","X"]) )
                result = r `explicit_join` (EQs ([[-1,1]],[-1],["I","X"]) )
                r = (EQs ([[1,0],[0,1]],[1,0],["I","X"]))

    i4d = testCase "[KarrDomain Test][i4c] eliminazione annichilente STEP 4" (assertEqual "" expected result) 
            where
                expected = [[-1,-1,1,0,0]]
                result = log_elimination aug_matrix 2 -- STEP4
                aug_matrix = [[1.0,1.0,0.0,-1.0,0.0],[0.0,0.0,1.0,-1.0,0.0]]

    i5 = testCase "[KarrDomain Test][i5] r join sys1;" (assertEqual "" expected result) 
            where
                sys1 = EQs ([[1,0],[0,1]],[2,1],["I","X"]) 
                sys0 = EQs ([[1,0],[0,1]],[1,0],["I","X"]) 
                result = sys0 `join` sys1
                expected = EQs ([[-1,1]],[-1],["I","X"]) 

    i6 = testCase "[KarrDomain Test][i6] r join bottom;" (assertEqual "" expected result) 
            where
                sys0 = EQsBottom
                sysR = EQs ([[1,0],[0,1]],[1,0],["I","X"]) 
                result = sysR `join` sys0
                expected = EQs ([[1,0],[0,1]],[1,0],["I","X"]) 

    i7 = testCase "[KarrDomain Test][i7] I<=1000" (assertEqual "" expected result) 
            where
                expected =  EQs ([[0,1]],[0],["I","X"])
                result = semS False progTree  sysR
                [(progTree,_)] = parse parseStms source_code 
                source_code = "if I<=1000 then I:=1 else I:=0" 
                sysR = EQs ([[1,0],[0,1]],[1,0],["I","X"]) 

    i8 = testCase "[KarrDomain Test][i8]  join" (assertEqual "" expected result) 
            where
                sysA = EQs ([[-1,1]],[-1],["I","X"]) 
                sysB = EQs ([],[],["I","X"]) 
                result = sysA `join` sysB
                expected = sysB



    tests = [a0,a1,c1,c2,c3,d1,d2,d3,d4,e1,e2,f1,f2,g1,g2,g3,g4,g5,g6,g7,g8,h1,h2,h3,h4,h5,h6,h7,
            i1,i1,i2,i3,i4,i4a,i4b,i4c,i5,i6,i7,i8,i4d]