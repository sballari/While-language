module AbsEval where
    import WhileStructures
    import AbsDomain
    import AbsState

    --abstract semantic of expressions in a non-relational domain
    exprE :: (AbsDomain a) => AExpr -> AbsState a -> a
    exprE (Var x) s = lookUp s x 
    exprE (Num n) _ = soundC (Num n)
    exprE (Range x y) _ = soundRange (Range x y)
    exprE (Minus e) s = absMinus (exprE e s)
    exprE (Sum e1 e2) s = absSum (exprE e1 s) (exprE e2 s)
    exprE (Mul e1 e2) s = absMul (exprE e1 s) (exprE e2 s)
    exprE (Div e1 e2) s = absDiv (exprE e1 s) (exprE e2 s)

    assS :: (AbsDomain a) => Stm -> AbsState a -> AbsState a
    assS (Assign var e) s   
        | s == Bottom           = Bottom
        | (exprE e s) == bottom = Bottom
        | otherwise             = alter s var (exprE e s)

    condC :: (AbsDomain a) => BExpr -> [AbsState a] -> [AbsState a] 
    --fina ultra grossa p. 54
    condC _ ss = ss


       


