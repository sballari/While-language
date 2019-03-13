module AbsCfgSem where
    import AbsEval
    import AbsDomain
    import AbsState as AS
    import WhileStructures
    import CFG

    type W = [Label] --widening points 
    
    
    --invariants' :: CGraph -> AbsState -> [(Label,AbsState)]
    -- at each program point i, we have a variable X#i with value in AbsState.
    -- I is a sound abs of the initial states (the 2nd parameter)

    -- invariants' cfg i =  
    --     where ls = labels cfg

    program1 :: CGraph a
    program1 = 
        [(L 1,id,L 2),
        (L 2,id,L 3),
        (L 3,id,L 4),
        (L 3,id,L 7),
        (L 6,id,L 3),
        (L 4,id,L 5),
        (L 5,id,L 6)]


    labels :: CGraph a-> [Label]
    labels cfg = [L i|i <- [0..m]]
        where L m = maximum (concat [[li,lj]|(li,_,lj)<-cfg])

    type Adjs a = [(Label,[(Label,a)])] -- lista delle liste di adiacenza
    inEdge :: CGraph a -> Adjs (AbsState a -> AbsState a)
    inEdge cfg = [(lj,[(li,f)|(li,f,lj)<-cfg]) | lj <-(labels cfg)] -- O(m+n*m): se denso n^3


   
    -- singola iterazione clm e' il risultato precedente
    type Clm a = [(Label, a)]
    invariant :: (AbsDomain a) => 
        Adjs (AbsState a -> AbsState a) -- grafo visto con lista adiacenze in entrata (+ comodo)
        -> [Label] --insieme delle label nel cfg
        -> W -- punti di widening
        -> Clm (AbsState a) -- risultato del calcolo precedente (iterazione k)
        -> Clm (AbsState a) -- iterazione k+1
    invariant in_adj labels ws clm = 
        do 
            li <- labels
            case lookup li in_adj of
                Just in_adj_li -> 
                        let 
                            Just xik = lookup li clm 
                            union = inUnion xik in_adj_li
                        in
                            if elem li ws then 
                                return (li, xik `AS.widening` union)
                            else return (li, union )
            

                
     

    inUnion :: AbsDomain a => AbsState a -> [(Label,AbsState a -> AbsState a)] -> AbsState a
    inUnion xik = foldr (\(lj,f) sr-> sr `AS.join` (f xik) ) Bottom 
    --in_adj_li = (i,f,j) in cfg[p]


    -- invariant :: (AbsDomain a) => CGraph a -> Label -> Int -> AbsState a -> W -> AbsState a
    -- invariant cfg li 0 is ws = Bottom
    -- invariant cfg (L 1) (k+1) is ws = is
    -- invariant cfg lj (k+1) is ws 
    --     | elem lj ws     =  
    --         let 
    --             xjk = (invariant cfg lj k)
    --             xik = (invariant cfg li k)
    --         in 
    --             xjk `widening` (foldr (\(li,f) sr-> sr `join` (f xik) ) Bottom inEdge)
    --     | otherwise      =  foldr (\(li,f) sr-> sr `join` (f xik) ) Bottom inEdge
    --     where 
    --         inEdge lj = [(li,f)|(li,f,lj)<-cfg]

