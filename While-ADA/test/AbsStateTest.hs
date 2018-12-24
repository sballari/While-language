module AbsStateTest (tests) where
    import AbsState
    import WhileStructures as WS
    import Test.Tasty
    import Test.Tasty.HUnit
    

    tests = [testAlter, testAlter1, testAlter2, testLookup, testLookup1]

    testAlter  = testCase "test alter 1" (assertEqual "" expected result)
        where 
            expected = [("x",21),("y",30),("z",11)]
            result = alter [("x",18),("y",30),("z",11)] "x" 21

    testAlter1  = testCase "test alter 2" (assertEqual "" expected result)
        where 
            expected = [("x",18),("y",30),("z",11),("j",21)]
            result = alter [("x",18),("y",30),("z",11)] "j" 21

    testAlter2  = testCase "test alter 3" (assertEqual "" expected result)
        where 
            expected = [("x",21),("x",30),("z",11)]
            result = alter [("x",18),("x",30),("z",11)] "x" 21
    

    instance UndefSup Int where 
        undef = 0


    testLookup  = testCase "test lookup 1" (assertEqual "" expected result)
        where 
            expected = 18 :: Int
            result = lookUp [("x",18),("y",30),("z",11)] "x" ::Int

    testLookup1  = testCase "test lookup 2" (assertEqual "" expected result)
        where 
            expected = 0 :: Int
            result = lookUp [("x",18),("y",30),("z",11)] "j" ::Int


    