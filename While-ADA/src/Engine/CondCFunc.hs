module CondCFunc where
    import AbsDomain as AD
    import AbsState as AS
    import WhileStructures
    import AbsEval

    type CondFun a = BExpr -> AbsState a -> AbsState a

{-  ##################################
    ##### COND GENERICA          #####
    ################################## -}

    condC :: (AbsDomain a) => CondFun a 
    --fina ultra grossa p. 54
    condC _ Bottom = Bottom
    condC (WTrue) s = s
    condC (WFalse) s = Bottom
    condC (And c1 c2) s = AS.meet (condC c1 s) (condC c2 s)
    condC (Or c1 c2) s =  AS.join (condC c1 s) (condC c2 s)
    condC _ s = s 