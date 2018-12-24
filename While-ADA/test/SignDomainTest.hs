module SignDomainTest (tests) where
    import AbsDomain
    import WhileStructures as WS
    import Test.Tasty
    import Test.Tasty.HUnit

    tests = [testUnion, testUnion1, testUnion2]

    testUnion  = testCase "union bottom eq0" (assertEqual "" expected result)
        where 
            expected = Eq0
            result = union Bottom Eq0

    testUnion1  = testCase "union Not0 Less0" (assertEqual "" expected result)
        where 
            expected = Top
            result = union Not0 Less0

    testUnion2  = testCase "union lessEq More" (assertEqual "" expected result)
        where 
            expected = Top
            result = union LessEq0 More0