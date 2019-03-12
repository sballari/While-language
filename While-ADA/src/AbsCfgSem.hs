module AbsCfgSem where
    import AbsEval
    import AbsDomain
    import AbsState as AS
    import WhileStructures
    import CFG

    type W = [Label] --widening points 
    
    
    invariants' :: CGraph -> AbsState -> [(Label,AbsState)]
    -- at each program point i, we have a variable X#i with value in AbsState.
    -- I is a sound abs of the initial states (the 2nd parameter)

    invariants' cfg i =  
        where ls = labels cfg

    invariant :: (AbsDomain a) => CGraph a-> Label -> Int -> AbsState a -> W -> AbsState a
    invariant cfg li 0 is ws = Bottom
    invariant cfg (L 1) (k+1) is ws = is
    invariant cfg lj (k+1) is ws 
        | elem lj ws     =  
            let 
                xjk = (invariant cfg lj k)
                xik = (invariant cfg li k)
            in 
                xjk `widening` (foldr (\(li,f) sr-> sr `join` (f xik) ) Bottom inEdge)
        | otherwise      =  foldr (\(li,f) sr-> sr `join` (f xik) ) Bottom inEdge
        where 
            inEdge = [(li,f)|(li,f,lj)<-cfg]

