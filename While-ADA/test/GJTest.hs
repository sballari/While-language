module GJTest where
    import KarrDomain as KR 
    import Test.Tasty
    import Test.Tasty.HUnit

    tests = [a1]

    a1 = testCase "[GJtest] [[1,2,3],[4,5,6],[7,8,9]], [5,5,5]" (assertEqual "" expected result)
        where 
            expected = 2
            result = 1