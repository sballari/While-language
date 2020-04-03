module MinimizePTest where
    import PolyUtils as PU
    import Test.Tasty
    import Test.Tasty.HUnit

    tests = [mul0,mul1,c1]

    mul0 = testCase "[Minimize][m0] (2+3x)*(y+2+2z)" (assertEqual "" expected result) 
        where
            expected = Nothing
            result = multiply [MC 2, M 3 "x"] [M 1 "y",MC 2, M 2 "z"]
                
    mul1 = testCase "[Minimize][m1] (2+3)*(y+2+2z)" (assertEqual "" expected result) 
        where
            expected = Just [M 2 "y", MC 4, M 4 "z", M 3 "y", MC 6, M 6 "z"]
            result = multiply [MC 2, MC 3] [M 1 "y",MC 2, M 2 "z"]
    
    c1 = testCase "[Minimize][c1] compact (2+3x+y+2+2y)" (assertEqual "" expected result) 
        where
            expected = [MC 4,M 3 "x", M 3 "y"]
            result = compact [MC 2, M 3 "x", M 1 "y", MC 2, M 2 "y"]