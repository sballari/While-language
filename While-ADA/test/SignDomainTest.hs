module SignDomainTest (tests) where
    import SignDomain as SD
    import AbsState as AS
    import AbsDomain as AD
    import WhileStructures as WS
    import Test.Tasty
    import Test.Tasty.HUnit

    tests = [testUnion, testUnion1, testUnion2,testInt, testInt1, testStateUnion, testStateUnion1,testStateInt1,testStateInt2 ]

    testUnion  = testCase "union bottom eq0" (assertEqual "" expected result)
        where 
            expected = Eq0
            result = AD.union SD.Bottom Eq0

    testUnion1  = testCase "union Not0 Less0" (assertEqual "" expected result)
        where 
            expected = Top
            result = AD.union Not0 Less0

    testUnion2  = testCase "union lessEq More" (assertEqual "" expected result)
        where 
            expected = Top
            result = AD.union LessEq0 More0

    testInt  = testCase "int" (assertEqual "" expected result)
            where 
                expected = Eq0
                result = AD.intersection LessEq0 MoreEq0
    
    testInt1  = testCase "int 1" (assertEqual "" expected result)
            where 
                expected = More0
                result = AD.intersection More0 MoreEq0

    testStateUnion = testCase "component wise union" (assertEqual "" expected result)
        where 
            expected = (S[("x",Top),("y",Eq0)])
            result = AS.union (S[("x",LessEq0),("y",SD.Bottom)]) (S[("x",More0),("y",Eq0)])


    testStateUnion1 = testCase "component wise union1" (assertEqual "" expected result)
        where 
            expected = (S[("x",Top),("y",Eq0)])
            result = AS.union (S[("x",LessEq0),("y",SD.Bottom),("z",Eq0)]) (S[("x",More0),("y",Eq0)])

    testStateInt1 = testCase "component wise int1" (assertEqual "" expected result)
        where 
            expected = AS.Bottom
            result = AS.intersection (S[("x",LessEq0),("y",SD.Bottom),("z",Eq0)]) (S[("x",More0),("y",Eq0)])

    testStateInt2 = testCase "component wise int2" (assertEqual "" expected result)
        where 
            expected = S [("x",Eq0), ("y",More0)]
            result = AS.intersection (S[("x",LessEq0),("y",More0),("z",Eq0)]) (S[("x",MoreEq0),("y",MoreEq0)])

