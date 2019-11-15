module KarrDomainTest where
    import KarrDomain as KD
    import Test.Tasty
    import Test.Tasty.HUnit

    tests = [echo_test]

    echo_test = testCase "[KarrDomain Test] echo_test" (assertEqual "" expected result) 
        where 
            expected = "ciao"
            result = "ciao"