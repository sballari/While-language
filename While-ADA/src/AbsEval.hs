module AbsEval where
    import WhileStructures
    import AbsDomain as AD
    import AbsState as AS

    --abstract semantic of expressions

    exprE ::  (AbsDomain a) => AExpr -> AbsState a -> a
    exprE (Var x) s = lookUp s x 
    exprE (Num n) _ = soundC n
    exprE (Range x y) _ = soundRange (x, y)
    exprE (Minus e) s = absMinus (exprE e s)
    exprE (Sum e1 e2) s = absSum (exprE e1 s) (exprE e2 s)
    exprE (Mul e1 e2) s = absMul (exprE e1 s) (exprE e2 s)
    exprE (Div e1 e2) s = absDiv (exprE e1 s) (exprE e2 s)

    -- semantica assegnazione: usata da semS e da AbsCfgSem
    semSG :: (AbsDomain a) => Stm -> AbsState a -> AbsState a
    semSG (Assign var e) s      
                    | s == Bottom           = Bottom
                    | (exprE e s) == bottom = Bottom
                    | otherwise             = alter s var (exprE e s)