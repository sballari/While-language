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

    signCondC :: CondFun Sign
    signCondC _ Bottom = Bottom
    signCondC (WTrue) s = s
    signCondC (WFalse) s = Bottom
    signCondC (And c1 c2) s = AS.meet (signCondC c1 s) (signCondC c2 s)
    signCondC (Or c1 c2) s =  AS.join (signCondC c1 s) (signCondC c2 s) 

    signCondC (LessEq (Var v) y) s 
        | ay == Zero && (a1 == Zero || a1 == MoreEqZero) = alter s v Zero
        | ay == Zero && (a1 == SignTop || a1 == LessEqZero) = alter s v LessEqZero
        | ay == LessEqZero = alter s v LessEqZero
        | ay == MoreEqZero = s
        | ay == SignBottom = Bottom -- caso non fattibile per smeshed bottom
        | ay == SignTop = s
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

    signCondC(MoreEq (Var v) (Num 0)) s 
        | a1 == Zero || a1 == LessEqZero = alter s v Zero
        | a1 == SignTop || a1 == MoreEqZero = alter s v MoreEqZero
        where 
            a1 = exprE (Var v) s

    signCondC (MoreEq (Var v) (Var w)) s =
        (if elem aw [Zero, MoreEqZero] then alter s v MoreEqZero else s)
        `AS.meet`
        (if elem av [Zero, LessEqZero] then alter s w LessEqZero else s)
        where 
            av = exprE (Var v) s
            aw = exprE (Var w) s
    
    signCondC (LessEq x y) s = s

    --CODICE NOSTRO NON COPIATO
    -- signCondC (LessEq x y) s 
    --     | (a1 !=SignTop) && (a2 != SignTop) && (a1 `maggioreStr` a2) = Bottom
    --     | a1==bottom || a2==bottom = Bottom
    --     | otherwise = s
    --     where 
    --         a1 = exprE x s
    --         a2 = exprE y s

    -- maggioreStr :: Sign -> Sign -> Bool
    -- maggioreStr a b = (!(a (AD.<=) b)) && (a!=b) && (b (AD.<=) a)
    -------------------------------

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
