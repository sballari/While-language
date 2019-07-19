module AbsStateTest  where
    import AbsState as AS
    import WhileStructures as WS
    import Test.Tasty
    import SignDomain as SD
    import Test.Tasty.HUnit
    
    


    tests = [testAlter, testAlter1, testAlter2, testLookup, testLookup1,m1,m2,u1,u2]

    testAlter  = testCase "test alter 1" (assertEqual "" expected result)
        where 
            expected = (S[("x",Zero),("y",LessEqZero),("z",Zero)])
            result = alter (S[("x",MoreEqZero),("y",LessEqZero),("z",Zero)]) "x" Zero

    testAlter1  = testCase "test alter 2" (assertEqual "" expected result)
        where 
        expected = (S[("x",Zero),("y",MoreEqZero),("z",LessEqZero),("j",MoreEqZero)])
        result = alter (S[("x",Zero),("y",MoreEqZero),("z",LessEqZero)]) "j" MoreEqZero

    testAlter2  = testCase "test alter 3" (assertEqual "" expected result)
        where 
        expected = (S[("x",SignTop),("x",MoreEqZero),("z",LessEqZero)])
        result = alter (S[("x",Zero),("x",MoreEqZero),("z",LessEqZero)]) "x" SignTop
    
            
    


    testLookup  = testCase "test lookup 1" (assertEqual "" expected result)
        where 
            expected = Zero 
            result = lookUp (S[("x",Zero),("y",MoreEqZero),("z",LessEqZero)]) "x" 

    testLookup1  = testCase "test lookup 2" (assertEqual "" expected result)
        where 
            expected = SignTop 
            result = lookUp (S[("x",Zero),("y",MoreEqZero),("z",LessEqZero)]) "j"

    m1  = testCase "meet 1" (assertEqual "" expected result)
        where 
            expected = S[("x",Zero),("y",Zero),("z",Zero),("j",Zero)]
            result = AS.meet (S[("x",Zero),("y",LessEqZero),("z",LessEqZero)]) (S[("x",MoreEqZero),("y",MoreEqZero),("z",Zero),("j",Zero)]) 

    m2  = testCase "meet 2" (assertEqual "" expected result)
        where 
            expected = S[("x",Zero),("y",LessEqZero),("z",Zero),("j",SignTop)]
            result = AS.meet (S[("x",Zero),("y",LessEqZero),("z",LessEqZero)]) (S[("x",MoreEqZero),("z",Zero),("j",Zero)]) 

    u1  = testCase "join 1" (assertEqual "" expected result)
        where 
            expected = S[("x",MoreEqZero),("y",SignTop),("z",LessEqZero),("j",SignTop)]
            result = AS.join (S[("x",Zero),("y",LessEqZero),("z",LessEqZero)]) (S[("x",MoreEqZero),("y",MoreEqZero),("z",Zero),("j",Zero)]) 
    u2  = testCase "join 2" (assertEqual "" expected result)
        where 
            expected = S[("x",MoreEqZero),("y",SignTop),("z",LessEqZero),("j",SignTop)]
            result = AS.join (S[("x",Zero),("y",LessEqZero),("z",LessEqZero)]) (S[("x",MoreEqZero),("z",Zero),("j",Zero)]) 