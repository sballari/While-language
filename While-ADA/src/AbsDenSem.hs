module AbsDenSem where
    import AbsEval
    import AbsDomain
    import AbsState as AS
    import WhileStructures

    semS :: (AbsDomain a) => CondFun a -> Stm -> AbsState a -> AbsState a
    semS condC (Assign var e) s = semSG (Assign var e) s
    semS condC (Assert b) s = condC b s
    semS condC (Skip) s = s
    semS condC (Comp stm1 stm2) s= ((semS condC stm2).(semS condC stm1)) s
    semS condC (Cond c s1 s2) s = AS.join p1 p2
        where 
            p1 = semS condC s1 (condC c s)
            p2 = semS condC s2 (condC (Neg c) s)

    semS condC (While c e) r = condC (Neg c) (lim fun)
        where fun x = AS.widening x (AS.join r (semS condC e (condC c x)))
    
    
    lim ::(AbsDomain a) => (AbsState a -> AbsState a) ->  AbsState a
    lim f = (lim' f 0) Bottom

    lim' ::(AbsDomain a) => (AbsState a -> AbsState a) -> Int -> (AbsState a -> AbsState a)
    lim' f 0 = if (id Bottom) == (f Bottom) then id else lim' f 1
    lim' f n = if  (f Bottom) == ((f.f) Bottom) then f
            else lim' (f.f) (n+1)