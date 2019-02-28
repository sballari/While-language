module AbsEval where
    import WhileStructures
    import AbsDomain as AD
    import AbsState as AS

    --abstract semantic of expressions in a non-relational domain

    
    exprEG ::  (AbsDomain a) => AExpr -> AbsState a -> a
    exprEG (Var x) s = lookUp s x 
    exprEG (Num n) _ = soundC (Num n)
    exprEG (Range x y) _ = soundRange (Range x y)
    exprEG (Minus e) s = absMinus (exprEG e s)
    exprEG (Sum e1 e2) s = absSum (exprEG e1 s) (exprEG e2 s)
    exprEG (Mul e1 e2) s = absMul (exprEG e1 s) (exprEG e2 s)
    exprEG (Div e1 e2) s = absDiv (exprEG e1 s) (exprEG e2 s)

    semSG :: (AbsDomain a) => Stm -> AbsState a -> AbsState a
    semSG (Assign var e) s      
                    | s == Bottom           = Bottom
                    | (exprEG e s) == bottom = Bottom
                    | otherwise             = alter s var (exprEG e s)

    condCG :: (AbsDomain a) => BExpr -> AbsState a -> AbsState a 
    --fina ultra grossa p. 54
    condCG (WTrue) s = s
    condCG (WFalse) s = Bottom
    condCG (And c1 c2) s = AS.meet (condCG c1 s) (condCG c2 s)
    condCG (Or c1 c2) s =  AS.join (condCG c1 s) (condCG c2 s)
    condCG _ s = s -- super approssimato


    


       


