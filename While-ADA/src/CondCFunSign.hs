module CondCFunSign where
    import AbsDomain as AD
    import AbsState as AS
    import SignDomain
    import WhileStructures
    import AbsEval
    import CondCFunc

{-  ##################################
    ##### COND PER DOMINIO SEGNI #####
    ################################## -} 

    signCondC :: CondFun Sign -- BExpr -> AbsState Sign -> AbsState Sign --
    signCondC (WTrue) s = s
    signCondC (WFalse) s = Bottom
    signCondC (And c1 c2) s = AS.meet (signCondC c1 s) (signCondC c2 s)
    signCondC (Or c1 c2) s =  AS.join (signCondC c1 s) (signCondC c2 s)


    signCondC (LessEq (Var v) (Var w)) s
        | av == SignBottom || aw == SignBottom = Bottom
        | otherwise = (if elem aw [Zero, LessEqZero] then (alter s v LessEqZero) else s)
                        `AS.meet`
                      (if elem av [Zero, MoreEqZero] then (alter s w MoreEqZero) else s)
        where 
            av = exprE (Var v) s
            aw = exprE (Var w) s

    signCondC (LessEq (Var v) y) s
        | a1 == SignBottom || ay == SignBottom = Bottom
        | ay == Zero && (a1 == MoreEqZero) = alter s v Zero
        | ay == Zero && (a1 == SignTop) = alter s v LessEqZero
        | ay == LessEqZero && a1 == SignTop = alter s v LessEqZero
        | ay == LessEqZero && a1 == MoreEqZero = alter s v Zero
        -- | ay == MoreEqZero = s
        -- | ay == SignTop = s
        | otherwise = s -- collasso tutti gli altri casi all'identità {ay = Zero e a1 == LessEqZero; ay = Zero e a1 == Zero}
        where 
            a1 = exprE (Var v) s
            ay = exprE y s

    signCondC (MoreEq (Var v) (Var w)) s
        | av == SignBottom || aw == SignBottom = Bottom
        | otherwise = (if elem aw [Zero, MoreEqZero] then alter s v MoreEqZero else s)
                        `AS.meet`
                      (if elem av [Zero, LessEqZero] then alter s w LessEqZero else s)
        where 
            av = exprE (Var v) s
            aw = exprE (Var w) s

    signCondC(MoreEq (Var v) y) s
        | a1 == SignBottom || ay == SignBottom = Bottom
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
    
    signCondC (LessEq x y) s
        | ax == SignBottom || ay == SignBottom = Bottom
        | otherwise = s
        where
            ax = exprE x s
            ay = exprE y s

    signCondC (Eq (Var x) y) s 
        | ay == SignBottom = Bottom
        | ax == LessEqZero && ay == MoreEqZero =  alter s x Zero
        | ax == MoreEqZero && ay == LessEqZero =  alter s x Zero
        | otherwise = s
        where 
            ax = exprE (Var x) s
            ay = exprE y s

    signCondC (Eq (Var x) (Var y)) s 
        | ay == SignBottom || ax == SignBottom = Bottom
        | ax == LessEqZero && ay == MoreEqZero =  alter (alter s x Zero) y Zero
        | ax == MoreEqZero && ay == LessEqZero =  alter (alter s x Zero) y Zero
        | otherwise = s
        where 
            ax = exprE (Var x) s
            ay = exprE (Var y) s

    signCondC (Eq x y) s 
        | ax == SignBottom || ay == SignBottom = Bottom
        | otherwise = s
        where 
            ax = exprE x s
            ay = exprE y s

    signCondC (NotEq x y) s 
        | ax == SignBottom || ay == SignBottom = Bottom 
        | ax == Zero || ay == Zero = Bottom 
        | otherwise = s
        where 
            ax = exprE x s
            ay = exprE y s    

    signCondC (MoreEq x y) s = signCondC (LessEq y x) s
    signCondC (Less x y) s = (signCondC (LessEq x y) s) `AS.meet` (signCondC (NotEq x y) s) --TODO PROP non bueno
    signCondC (More x y) s = (signCondC (MoreEq x y) s) `AS.meet` (signCondC (NotEq x y) s) --
    12:38 PM

    --------------------------------
    -- CONTROLLARE -----------------
    --------------------------------
    signCondC (Neg WTrue ) s = signCondC WFalse s
    signCondC (Neg WFalse ) s = signCondC WTrue s
    signCondC (Neg (Eq a b)) s = signCondC  (NotEq a b) s
    signCondC (Neg (LessEq a b)) s = signCondC (More a b) s
    signCondC (Neg (Less a b)) s = signCondC (MoreEq a b) s
    signCondC (Neg (NotEq a b)) s = signCondC (Eq a b) s
    signCondC (Neg (More a b)) s = signCondC (LessEq a b) s
    signCondC (Neg (MoreEq a b)) s = signCondC (Less a b) s
    signCondC (Neg (Neg b)) s = signCondC b s
    signCondC (Neg (And a b) ) s = signCondC (Or (Neg a) (Neg b)) s
    signCondC (Neg (Or a b) ) s = signCondC (And (Neg a) (Neg b)) s

    signCondC b s = s