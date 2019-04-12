module AbsDenSem where
    import AbsEval
    import AbsDomain
    import AbsState as AS
    import WhileStructures
    import CondCFunc

    type Widening = Bool -- fare widening?

    semS :: (AbsDomain a) => Widening -> CondFun a -> Stm -> AbsState a -> AbsState a
    --Widening : true se si vuole widening, false se non lo si vuole usare
    semS w condC (Assign var e) s = semSG (Assign var e) s
    semS w condC (Assert b) s = condC b s
    semS w condC (Skip) s = s
    semS w condC (Comp stm1 stm2) s= ((semS w condC stm2).(semS w condC stm1)) s
    semS w condC (Cond c s1 s2) s = AS.join p1 p2
        where 
            p1 = semS w condC s1 (condC c s)
            p2 = semS w condC s2 (condC (Neg c) s)

    semS w condC (While c e) r = condC (Neg c) (lim fun)
        where fun x = if w==True then AS.widening x (AS.join r (semS w condC e (condC c x)))
                        else AS.join r (semS w condC e (condC c x))
    
    
    lim ::(AbsDomain a) => (AbsState a -> AbsState a) ->  AbsState a
    lim f = (lim' f 0) Bottom

    lim' ::(AbsDomain a) => (AbsState a -> AbsState a) -> Int -> (AbsState a -> AbsState a)
    lim' f 0 = if (id Bottom) == (f Bottom) then id else lim' f 1
    lim' f n = if  (f Bottom) == ((f.f) Bottom) then f
            else lim' (f.f) (n+1)