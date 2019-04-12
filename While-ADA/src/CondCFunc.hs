module CondCFunc where
    import AbsDomain as AD
    import AbsState as AS
    import SignDomain
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
    condC _ s = s -- super approssimato

{-  ##################################
    ##### COND PER DOMINIO SEGNI #####
    ################################## -} 

    signCondC :: CondFun Sign -- BExpr -> AbsState Sign -> AbsState Sign --
    signCondC _ Bottom = Bottom
    signCondC (WTrue) s = s
    signCondC (WFalse) s = Bottom
    signCondC (And c1 c2) s = AS.meet (signCondC c1 s) (signCondC c2 s)
    signCondC (Or c1 c2) s =  AS.join (signCondC c1 s) (signCondC c2 s)

    signCondC (LessEq (Var v) y) s
        | ay == Zero && ({-a1 == Zero ||-} a1 == MoreEqZero) = alter s v Zero
        | ay == Zero && (a1 == SignTop {-|| a1 == LessEqZero-}) = alter s v LessEqZero
        | ay == LessEqZero && a1 == SignTop = alter s v LessEqZero
        | ay == LessEqZero && a1 == MoreEqZero = alter s v Zero
        {-| ay == MoreEqZero = s-} 
        {-| ay == SignTop = s-}
        | otherwise = s -- collasso tutti gli altri casi all'identità
        where 
            a1 = exprE (Var v) s
            ay = exprE y s


    signCondC (LessEq (Var v) (Var w)) s =
        (if elem aw [Zero, LessEqZero] then (alter s v LessEqZero) else s)
        `AS.meet`
        (if elem av [Zero, MoreEqZero] then (alter s w MoreEqZero) else s)
        where 
            av = exprE (Var v) s
            aw = exprE (Var w) s

    signCondC(MoreEq (Var v) y) s
        | ay == Zero && (a1 == LessEqZero) = alter s v Zero
        | ay == Zero && (a1 == SignTop) = alter s v MoreEqZero
        | ay == MoreEqZero && a1 == SignTop = alter s v MoreEqZero
        | ay == MoreEqZero && a1 == LessEqZero = alter s v Zero
        {-| ay == LessEqZero = s-} 
        {-| ay == SignTop = s-}
        | otherwise = s -- collasso tutti gli altri casi all'identità
        where 
            a1 = exprE (Var v) s
            ay = exprE y s

    signCondC (MoreEq (Var v) (Var w)) s =
        (if elem aw [Zero, MoreEqZero] then alter s v MoreEqZero else s)
        `AS.meet`
        (if elem av [Zero, LessEqZero] then alter s w LessEqZero else s)
        where 
            av = exprE (Var v) s
            aw = exprE (Var w) s
    
    signCondC (LessEq x y) s = s

    signCondC (Eq x y) s 
        | ax == SignBottom || ay == SignBottom = Bottom
        | otherwise = s
        where 
            ax = exprE x s
            ay = exprE y s

    signCondC (NotEq x y) s 
        | ax == SignBottom || ay == SignBottom = Bottom 
        | otherwise = s
        where 
            ax = exprE x s
            ay = exprE y s

    signCondC (MoreEq x y) s = signCondC (LessEq y x) s
    signCondC (Less x y) s = (signCondC (LessEq x y) s) `AS.meet` (signCondC (NotEq x y) s)
    signCondC (More x y) s = (signCondC (MoreEq x y) s) `AS.meet` (signCondC (NotEq x y) s)
    signCondC b s = s
