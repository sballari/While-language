module IntervalDomainTest  where
    import IntervalDomain as SD
    import AbsState as AS
    import AbsDomain as AD
    import WhileParser
    import WhileStructures as WS
    import Test.Tasty
    import Test.Tasty.HUnit
    import AbsEval
    import AbsDenSem
    import CondCFunc
    

    tests = [a1,a2,a3]

    a1 = testCase "[Interval Domain] intcond R>=B" (assertEqual "" expected result)
        where 
            expected = S [("Q",Interval (B 0) (B 0)),("R",Interval (B 1) (B 150)),("B",Interval (B 1) (B 3)),("A",Interval (B 0) (B 150))]
            result = intCondC (MoreEq (Var "R") (Var "B")) state
            state =  S[("Q",Interval (B 0) (B 0)),("R",Interval (B 0) (B 150)),("B", Interval (B 1) (B 3)), ("A", Interval (B 0) (B 150))]
            
    a2 = testCase "[Interval Domain] intcond R<B" (assertEqual "" expected result)
        where 
            expected = S [("Q",Interval (B 0) (B 0)),("R",Interval MinInf (B 2)),("B",Interval (B 1) (B 3)),("A",Interval (B 0) (B 150))]
            result = intCondC (Less (Var "R") (Var "B")) state
            state =  S[("Q",Interval (B 0) (B 0)),("R",Interval MinInf (B 150)),("B", Interval (B 1) (B 3)), ("A", Interval (B 0) (B 150))]

    a3 = testCase "[Interval Domain] intcond R<=B" (assertEqual "" expected result)
        where 
            expected = S [("Q",Interval (B 0) (B 0)),("R",Interval MinInf (B 3)),("B",Interval (B 1) (B 3)),("A",Interval (B 0) (B 150))]
            result = intCondC (Less (Var "R") (Var "B")) state
            state =  S[("Q",Interval (B 0) (B 0)),("R",Interval MinInf (B 150)),("B", Interval (B 1) (B 3)), ("A", Interval (B 0) (B 150))]