module AbsEval where
    import WhileStructures
    import AbsDomain as AD
    import AbsState as AS

    --abstract semantic of expressions in a non-relational domain
    exprE :: (AbsDomain a) => AExpr -> AbsState a -> a
    exprE (Var x) s = lookUp s x 
    exprE (Num n) _ = soundC (Num n)
    exprE (Range x y) _ = soundRange (Range x y)
    exprE (Minus e) s = absMinus (exprE e s)
    exprE (Sum e1 e2) s = absSum (exprE e1 s) (exprE e2 s)
    exprE (Mul e1 e2) s = absMul (exprE e1 s) (exprE e2 s)
    exprE (Div e1 e2) s = absDiv (exprE e1 s) (exprE e2 s)

    semS :: (AbsDomain a) => Stm -> AbsState a -> AbsState a
    semS (Assign var e) s      
                    | s == Bottom           = Bottom
                    | (exprE e s) == bottom = Bottom
                    | otherwise             = alter s var (exprE e s)

    condC :: (AbsDomain a) => BExpr -> AbsState a -> AbsState a 
    --fina ultra grossa p. 54
    condC (WTrue) s = s
    condC (WFalse) s = Bottom
    condC (And c1 c2) s = AS.meet (condC c1 s) (condC c2 s)
    condC (Or c1 c2) s =  AS.join (condC c1 s) (condC c2 s)
    condC _ s = s


    


       


