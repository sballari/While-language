module AbsCfgSem where
    import AbsEval
    import AbsDomain
    import AbsState as AS
    import WhileStructures
    import CFG

    type W = [Label] --widening points 
    type Clm a = [(Label, a)]
    type Adjs a = [(Label,[(Label,a)])] 
    -- Grafo visto come lista delle liste di adiacenza in entrata (convertitore)

    ---------------------------------------------------
    ------------ GRAPH UTILITY FUNCTIONS  -------------
    ---------------------------------------------------

    labels :: Graph a -> [Label]
    labels cfg = [L i|i <- [0..m]]
        where L m = maximum (concat [[li,lj]|(li,_,lj)<-cfg])

    in_adjs :: Graph a -> Adjs a
    in_adjs cfg = [(lj,[(li,f)|(li,f,lj)<-cfg]) | lj <-(labels cfg)] -- O(m+n*m): se denso n^3

    ---------------------------------------------------
    --------------------- FINE  -----------------------
    ---------------------------------------------------
    analyze :: AbsDomain a =>
        CGraph a ->
        W -> -- punti di widening
        AbsState a -> -- initial state of the program
        Clm (AbsState a) -- invarianti per ogni program point
    analyze cfg ws is = 
        invariantCalc inGraph ls ws is clm0
        where 
            ls = labels cfg  
            clm0 = [(li,Bottom)|li <- ls] 
            inGraph = in_adjs cfg

    invariantCalc :: AbsDomain a =>
        Adjs (AbsState a -> AbsState a) ->
        [Label] -> -- label del programma
        W -> -- punti di widening
        AbsState a -> -- initial state of the program
        Clm (AbsState a) -> 
        Clm (AbsState a) -- invarianti per ogni program point
    invariantCalc inGraph ls ws is clm= 
        if iter clm == clm then clm -- trovato un pt fisso
        else invariantCalc inGraph ls ws is clm
        where
            iter = invariant inGraph ls ws is
   
    -- singola iterazione clm e' il risultato precedente
    invariant :: (AbsDomain a) => 
        Adjs (AbsState a -> AbsState a) -- grafo visto con lista adiacenze in entrata (+ comodo)
        -> [Label] -- insieme delle label nel cfg (per semplicita')
        -> W -- punti di widening
        -> AbsState a -- initial state
        -> Clm (AbsState a) -- risultato del calcolo precedente (iterazione k)
        -> Clm (AbsState a) -- iterazione k+1
    invariant in_adj labels ws is clm = 
        do 
            lj <- labels
            if lj == L 1 then return (lj, is)
            else 
                case lookup lj in_adj of
                    Just in_adj_lj -> 
                            let 
                                Just xjk = lookup lj clm 
                                union = inUnion clm in_adj_lj
                            in
                                if elem lj ws then 
                                    return (lj, xjk `AS.widening` union)
                                else return (lj, union )
            

                
    inUnion :: AbsDomain a => 
        Clm (AbsState a)  -- risultati iterazione k per ogni program point
        -> [(Label,AbsState a -> AbsState a)] -- lista degli archi in entrata
        -> AbsState a --risultato dell'unione
    inUnion clm = 
        foldr   (\(li,f) sr-> 
                    sr `AS.join` 
                    ( let Just xik= lookup li clm in  (f xik) )
                ) Bottom 
    -- calcole della 
    -- in_adj_li = (i,f,j) in cfg[p] (archi entranti in Li)