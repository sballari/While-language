module KarrDomainTest where
    import KarrDomain as KD
    import AbsDomainR
    import Test.Tasty
    import Test.Tasty.HUnit

    tests = [a0,a1]

    a0 = testCase "[KarrDomain Test][a0] join (ex5.5 p109)" (assertEqual "" expected result) 
        where
            expected = EQs ([
                            [1,1,0,0,0,0,0,-3],
                            [0,0,0,0,0,0,1,1],
                            [0,0,1,0,0,0,0,1],
                            [0,0,0,1,0,0,0,10],
                            [0,0,0,0,1,0,0,-2],
                            [0,0,0,0,0,1,0,-12]]
                            ,[11,1,1,10,0,0])
            result = join p1 p2
            p1 = EQs ([[1,0],[0,1]],[1,10]) -- programs s.t. x1 = 1 and x2 = 10
            p2 = EQs ([[1,0],[0,1]],[2,12]) -- programs s.t. x1 = 2 and x2 = 12

    a1 = testCase "[KarrDomain Test][a1] explicit_join (ex5.5 p109)" (assertEqual "" expected result) 
        where
            expected = EQs ([
                [-1,-1,1,1,1,1,0,0],
                [0,0,0,0,0,0,1,1],
                [0,0,1,0,0,0,-1,0],
                [0,0,0,1,0,0,-10,0],
                [0,0,0,0,1,0,0,-2],
                [0,0,0,0,0,1,0,-12]]
                ,[0,1,0,0,0,0])
            result = explicit_join p1 p2
            p1 = EQs ([[1,0],[0,1]],[1,10]) -- programs s.t. x1 = 1 and x2 = 10
            p2 = EQs ([[1,0],[0,1]],[2,12]) -- programs s.t. x1 = 2 and x2 = 12