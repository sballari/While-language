module AbsDenSem where
    import AbsEval
    import AbsDomain
    import AbsState as AS
    import WhileStructures
    import CondCFunc
    import Control.Monad.Trans.State.Lazy

    type Widening = Bool -- fare widening?

    semS :: (AbsDomain a) => Widening -> CondFun a -> Stm -> AbsState a -> AbsState a
    semS w condC (stm) s = evalState (semS' stm s) (w, condC)
    -- semS w condC (Assign var e) s = semSG (Assign var e) s
    -- semS w condC (Assert b) s = condC b s
    -- semS w condC (Skip) s = s
    -- semS w condC (Comp stm1 stm2) s= ((semS w condC stm2).(semS w condC stm1)) s
    -- semS w condC (Cond c s1 s2) s = AS.join p1 p2
    --     where 
    --         p1 = semS w condC s1 (condC c s)
    --         p2 = semS w condC s2 (condC (Neg c) s)

    -- semS w condC (While c e) r = condC (Neg c) (lim fun)
    --     where fun x = if w==True then AS.widening x (AS.join r (semS w condC e (condC c x)))
    --                     else AS.join r (semS w condC e (condC c x))
    
    getW :: (AbsDomain a) => State (Widening, CondFun a) Widening
    getW =
        do 
            state <- get 
            return (fst state)
    getCondC :: (AbsDomain a) => State (Widening, CondFun a) (CondFun a) 
    getCondC =
        do 
            state <- get 
            return (snd state)

    semS' :: (AbsDomain a) => Stm -> AbsState a -> State (Widening, CondFun a) (AbsState a)
    semS' (Assign var e) s = return (semSG (Assign var e) s)
    semS' (Assert b) s = 
        do 
            condC <- getCondC
            return (condC b s)
    semS' (Skip) s = return s
    semS' (Comp stm1 stm2) s= 
        do 
            s1 <- semS' stm1 s
            s2 <- semS' stm1 s1
            return s2
    semS' (Cond c s1 s2) s = 
        do 
            condC <- getCondC
            p1 <- semS' s1 (condC c s)
            p2 <- semS' s2 (condC (Neg c) s)
            return AS.join p1 p2

                
    semS' (While c e) r = 
        do 
            condC <- getCondC
            w <- getW
            return condC (Neg c) (lim (fun w))
        where fun x w = 
                if w then 
                    AS.widening x (AS.join r (semS' e (condC c x))) 
                else AS.join r (semS' e (condC c x))

    lim ::(AbsDomain a) => (AbsState a -> AbsState a) ->  AbsState a
    lim f = (lim' f 0) Bottom

    lim' ::(AbsDomain a) => (AbsState a -> AbsState a) -> Int -> (AbsState a -> AbsState a)
    lim' f 0 = if (id Bottom) == (f Bottom) then id else lim' f 1
    lim' f n = if  (f Bottom) == ((f.f) Bottom) then f
            else lim' (f.f) (n+1)