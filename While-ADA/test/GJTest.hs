module GJTest where
    import KarrDomain as KR 
    import Test.Tasty
    import Test.Tasty.HUnit

    tests = [a0,a01,a1,a10,a11,a12,a2,a3,a4,a5,a5bis,a5ter,a5quad,a6,a7,a8,a9]

    a0 = testCase "[GJtest] Transpose" (assertEqual "" expected result) 
        where 
            expected = [[1,4,7],[2,5,8],[3,6,9]]
            result = rowM2colM ([[1,2,3],[4,5,6],[7,8,9]])

    a01 = testCase "[GJtest] Transpose1" (assertEqual "" expected result) 
        where 
            expected = [[1,4],[2,5],[3,6]]
            result = rowM2colM ([[1,2,3],[4,5,6]])

    a1 = testCase "[GJtest][a1] [[1,2,3,2],[1,2,4,4],[0,1,2,1]], [2,5,3]" (assertEqual "" expected result)
        where 
            expected = EQs([[ 1,2,3,2],[0,0,1,2],[0,1,0,-3]],[2,3,-3])
            result = gaussJordanEl (EQs (m,c))
            m = [[1,2,3,2],[1,2,4,4],[0,1,2,1]]
            c = [2,5,3]

    a10 = testCase "[GJtest] [[1,2,3,2]], [2]" (assertEqual "" expected result)
            where 
                expected = EQs (m,c)
                result = gaussJordanEl (EQs (m,c))
                m = [[1,2,3,2]]
                c = [2]
    a11 = testCase "[GJtest] [[0,1,2,1],[0,0,1,2]], [3,3]" (assertEqual "" expected result)
        where 
            expected = EQs([[0,1,2,1],[0,0,1,2]],[3,3])
            result = gaussJordanEl (EQs (m,c))
            m = [[0,1,2,1],[0,0,1,2]]
            c = [3,3]

    a12 = testCase "[GJtest][a12] [[0,1,2,1]], [3]" (assertEqual "" expected result)
        where 
            expected = EQs([[0,0,1,2]],[3])
            result = gaussJordanEl (EQs (m,c))
            m = [[0,0,1,2]]
            c = [3]

    a2 = testCase "[GJtest] zerofication [[1,2,3,2],[1,2,4,4],[0,1,2,1]], [2,5,3]" (assertEqual "" expected result)
        where 
            expected = [[0,0,1,2],[0,1,2,1]]
            result = zerofication ms m coef
            m:ms = [[1,2,3,2],[1,2,4,4],[0,1,2,1]]
            coef = [-1,0]

    a3 = testCase "[GJtest] firstNZCol [[1,2,3,2],[1,2,4,4],[0,1,2,1]]" (assertEqual "" expected result)
        where
            expected = Just (1,[1,0])
            result = firstNZCol [[1,2,3,2],[1,2,4,4],[0,1,2,1]]

    a4 = testCase "[GJtest] firstNZCol [[1,2,3,2],[1,2,4,4],[0,1,2,1]]" (assertEqual "" expected result)
        where
            expected = Just (2,[2,1])
            result = firstNZCol [[0,2,3,2],[1,2,4,4],[0,1,2,1]]

    a5 = testCase "[GJtest] const row zerofication" (assertEqual "" expected result)
        where
            expected = [3,3]
            [result] = transpose (zerofication (transpose [cs]) [c'] el_coef )
            c' = 2
            cs = [5,3]
            el_coef = [-1,0]

    
    a5bis = testCase "[GJtest] const row zerofication Manual" (assertEqual "" expected result)
        where
            expected = [[3],[3]]
            result = zerofication cs c' el_coef 
            c' = [2]
            cs = [[5],[3]]
            el_coef = [-1,0]

    a5ter = testCase "[GJtest] zerofication no row" (assertEqual "" expected result)
        where
            expected = []
            result = transpose (zerofication (transpose cs) [c'] el_coef )
            c' = 2
            cs = []
            el_coef = []

    a5quad = testCase "[GJtest] zerofication no row" (assertEqual "" expected result)
        where
            expected = []
            result = zerofication ms m el_coef
            m = [1,2,3]
            ms = []
            el_coef = []
            
    a6 = testCase "[GJtest] firstNZCol [[1,2,3]]" (assertEqual "" expected result)
            where
                expected = Just (2,[])
                result = firstNZCol [[0,2,3]]
    a7 = testCase "[GJtest][a7] zerofication [[0,1,2,1],[0,0,1,2]],[3,3]" (assertEqual "" expected result)
        where
            expected = ([[0.0,0.0,1.0,2.0]],[3])
            result = ((zerofication ms m el_coef),(cs'))
            el_coef = fmap (*(-1)) nz_col
            Just (nz_el,nz_col) = firstNZCol (m:ms)
            (m:ms) = [[0.0,1.0,2.0,1.0],[0.0,0.0,1.0,2.0]]
            (c:cs) = [3.0,3.0]
            c' = c/nz_el
            [cs']=transpose (zerofication (transpose [cs]) [c'] el_coef )

    a8 = testCase "[GJtest][a8] find el coef [[0,0,1,2],[0,1,2,1]]" (assertEqual "" expected result)
        where
            expected = (1,[2],[-2])
            result = (nz_el,nz_col,el_coef)
            el_coef = fmap (*(-1)) nz_col
            Just (nz_el,nz_col) = firstNZCol (m:ms)
            (m:ms) = [[0,0,1,2],[0,1,2,1]]

    a9 = testCase "[GJtest][a9] RE [[1,2,3,2],[1,2,4,4],[0,1,2,1]], [2,5,3]" (assertEqual "" expected result)
        where 
            expected = EQs([[ 1,0,0,2],[0,0,1,2],[0,1,0,-3]],[-1,3,-3])
            result = rowEchelonForm (EQs (m,c))
            m = [[1,2,3,2],[1,2,4,4],[0,1,2,1]]
            c = [2,5,3]