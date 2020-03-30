module AbsDenSemR where
    import AbsDomainR
    import WhileStructures

    type Widening = Bool --widening?

    semS :: (AbsDomainR a) => Widening ->  Stm -> a -> a
    semS w  (Assign var e) s = assignS (Assign var e) s
    semS w  (Assert b) s =  condC b s
    semS w  (Skip) s = s
    semS w  (Comp stm1 stm2) s = ((semS w stm2).(semS w stm1)) s
    semS w  (Cond c s1 s2) s = p1 `join` p2
        where 
            p1 = semS w  s1 (condC c s)
            p2 = semS w  s2 (condC (Neg c) s)
            
    semS w  (While c e) r =  condC (Neg c) (lim fun)
        where fun x = 
                if w 
                then 
                    let s =  r `join` (semS w  e (condC c x)) in 
                        x `widening` s
                else 
                    r `join` (semS w  e (condC c x))


    lim ::(AbsDomainR a) => (a -> a) ->  a
    lim f = (lub [pow n f | n <- [0..]]) bottom
        where pow 0 f = id
              pow n f = f.(pow (n-1) f)
        
    lub (f:f':fs) = 
        if f (bottom) == f' bottom
        then f
        else lub fs