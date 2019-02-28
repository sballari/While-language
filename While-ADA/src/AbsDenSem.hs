module AbsDenSem where
    import AbsEval
    import AbsDomain
    import AbsState as AS
    import WhileStructures



    class (AbsEval a) => AbsDenSem a where 

        exprE ::  AExpr -> AbsState a -> a
        exprE e s = exprEG e s

        condC ::  BExpr -> AbsState a -> AbsState a 
        condC bexpr s = condCG bexpr s

        semS ::  Stm -> AbsState a -> AbsState a
        semS (Assign var e) s = semSG (Assign var e) s
        semS (Assert b) s = condC b s
        semS (Skip) s = s
        semS (Comp stm1 stm2) s= ((semS stm2).(semS stm1)) s
        semS (Cond c s1 s2) s = AS.join p1 p2
            where 
                p1 = semS s1 (condC c s)
                p2 = semS s2 (condC (Neg c) s)

        semS (While c e) s = condC (Neg c) (lim fun)
            where fun x = AS.widening x (AS.join s (semS e (condC c x)))
        
        
        lim :: (AbsState a -> AbsState a) ->  AbsState a
        lim f = (lim' f 0) Bottom

        lim' :: (AbsState a -> AbsState a) -> Int -> (AbsState a -> AbsState a)
        lim' f 0= if (id Bottom) == (f Bottom) then id else lim' f 1
        lim' f n = if  (f Bottom) == ((f.f) Bottom) then f
                else lim' (f.f) (n+1)