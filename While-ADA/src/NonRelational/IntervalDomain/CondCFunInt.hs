module CondCFunInt where
    import AbsValueDomain as AVD
    import AbsDomain as AD
    import AbsState as AS
    import IntervalDomain
    import WhileStructures
    import CondCFunc
    import AbsEval
{- ##################################
    #### COND PER DOMINIO INTERVALLI ####
    ################################## -} 

    intCondC :: CondFun (AbsState Interval) 
    -- intCondC :: BExpr -> AbsState IntervalDomain -> AbsState IntervalDomain 
 
    intCondC _ bottom = Bottom
    intCondC (WTrue) s = s
    intCondC (WFalse) s = Bottom
    intCondC (And c1 c2) s = AD.meet (intCondC c1 s) (intCondC c2 s)
    intCondC (Or c1 c2) s =  AD.join (intCondC c1 s) (intCondC c2 s)

    intCondC (LessEq (Var v) (Var w)) s 
        | a Prelude.<= d = alter (alter s v (Interval a (min b d))) w (Interval (max a c) d)
        | otherwise = Bottom
        where
            Interval a b = lookUp s v
            Interval c d = lookUp s w

    intCondC (LessEq (Var v) (Num y)) s
            | a Prelude.<= (B y) = alter s v (Interval a (min b (B y)))
            | otherwise = Bottom
            where 
                Interval a b = lookUp s v

    intCondC (Less (Var v) (Var w)) s 
        | a Prelude.< d = alter (alter s v (Interval a rigth)) w (Interval left d)
        | otherwise = Bottom
        where
            Interval a b = lookUp s v
            Interval c d = lookUp s w
            rigth = if b Prelude.< d then b else d IntervalDomain.- (B 1)
            left = if a >= c then a IntervalDomain.+ (B 1) else  c

    intCondC (Less (Var v) (Num y)) s
            | a Prelude.< (B y) = alter s v (Interval a rigth)
            | otherwise = Bottom
            where 
                Interval a b = lookUp s v
                rigth = if b Prelude.< (B y) then b else (B y) IntervalDomain.- (B 1)
    
    intCondC (MoreEq (Var v) (Var w)) s 
        | b >= c = alter (alter s v (Interval (max c a) b)) w (Interval c (min d b))
        | otherwise = Bottom
        where
            Interval a b = lookUp s v
            Interval c d = lookUp s w
            

    intCondC(MoreEq (Var v) (Num y)) s
        | b >= (B y) = alter s v (Interval (max a (B y)) b)
        | otherwise = Bottom
        where 
            Interval a b = lookUp s v

    intCondC (More (Var v) (Var w)) s 
        | b > c = alter (alter s v (Interval left b)) w (Interval c rigth)
        | otherwise = Bottom
        where
            Interval a b = lookUp s v
            Interval c d = lookUp s w
            rigth = if a Prelude.> c then a else c IntervalDomain.+ (B 1)
            left = if b Prelude.<= d then b IntervalDomain.- (B 1) else d

    intCondC(More (Var v) (Num y)) s
        | b > (B y) = alter s v (Interval left b)
        | otherwise = Bottom
        where 
            Interval a b = lookUp s v
            left = if a Prelude.<= (B y) then B(y Prelude.+ 1) else a

    intCondC (Eq (Var x) (Var y)) s = 
        if int==IntervalBottom then Bottom else alter (alter s x int) y int
        where 
            ax = exprE (Var x) s
            ay = exprE (Var y) s
            int = AVD.meet ax ay

    intCondC (Eq (Var x) y) s = 
        if int==IntervalBottom then Bottom else alter s x int
        where 
            ax = exprE (Var x) s
            ay = exprE y s
            int = AVD.meet ax ay

    intCondC (Eq x (Var y)) s = 
        if int==IntervalBottom then Bottom else alter s y int
        where 
            ax = exprE x s
            ay = exprE (Var y) s
            int = AVD.meet ax ay

    intCondC (Eq x y) s = 
        if int==IntervalBottom then AD.bottom else s
        where 
            ax = exprE x s
            ay = exprE y s
            int = AVD.meet ax ay

    intCondC (NotEq (Var x) (Num n)) s 
        | (B n) ==a && (B n)==b  = Bottom 
        | a == (B n)    = alter s x (Interval (B(n Prelude.+1)) b)
        | b == (B n)    = alter s x (Interval a (B(n Prelude.-1))) 
        | otherwise     = s
        where 
            Interval a b = exprE (Var x) s
    
    intCondC (NotEq (Num n) (Var x)) s 
        | (B n)==a && (B n)==b  = Bottom 
        | a == (B n)    = alter s x (Interval (B (n Prelude.+1)) b)
        | b == (B n)    = alter s x (Interval a (B (n Prelude.-1))) 
        | otherwise     = s
        where 
            Interval a b = exprE (Var x) s

    intCondC (NotEq x y) s = s       
    intCondC (Neg WTrue ) s = intCondC WFalse s
    intCondC (Neg WFalse ) s = intCondC WTrue s
    intCondC (Neg (Eq a b)) s = intCondC  (NotEq a b) s
    intCondC (Neg (LessEq a b)) s = intCondC (More a b) s
    intCondC (Neg (Less a b)) s = intCondC (MoreEq a b) s
    intCondC (Neg (NotEq a b)) s = intCondC (Eq a b) s
    intCondC (Neg (More a b)) s = intCondC (LessEq a b) s
    intCondC (Neg (MoreEq a b)) s = intCondC (Less a b) s
    intCondC (Neg (Neg b)) s = intCondC b s
    intCondC (Neg (And a b) ) s = intCondC (Or (Neg a) (Neg b)) s
    intCondC (Neg (Or a b) ) s = intCondC (And (Neg a) (Neg b))   s
    
    
    intCondC b s = s

    