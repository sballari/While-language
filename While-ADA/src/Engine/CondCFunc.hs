module CondCFunc where
    --import AbsValueDomain as AVD
    import AbsDomain as AD
    import WhileStructures
    import AbsEval

    type CondFun a = BExpr -> a -> a

{-  ##################################
    ##### COND GENERICA          #####
    ################################## -}

    condC :: (AbsDomain a) => CondFun a
    --fina ultra grossa p. 54
    condC _ bottom = AD.bottom
    condC (WTrue) s = s
    condC (WFalse) s = AD.bottom
    condC (And c1 c2) s = AD.meet (condC c1 s) (condC c2 s)
    condC (Or c1 c2) s =  AD.join (condC c1 s) (condC c2 s)
    condC _ s = s 