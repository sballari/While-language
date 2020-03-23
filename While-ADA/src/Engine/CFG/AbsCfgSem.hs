module AbsCfgSem where
    import AbsEval
    import AbsValueDomain
    import AbsState as AS
    import WhileStructures
    import CFG

    type W = [Label] --widening points 
    type Clm a = [(Label, a)]
    
    analyze :: AbsValueDomain a =>
        CGraph a -> -- grafo di computazione
        W -> -- punti di widening
        AbsState a -> -- initial state of the program
        [Clm (AbsState a)] -- invarianti per ogni program point
    analyze cfg ws is = 
        invariantCalc inGraph ls ws is [clm0]
        where 
            ls = labels cfg  
            clm0 = [(li,Bottom)|li <- ls] 
            inGraph = in_adjs cfg

    invariantCalc :: AbsValueDomain a =>
        Adjs (AbsState a -> AbsState a) -> -- lista adiacenze in entrata
        [Label] -> -- label del programma
        W -> -- punti di widening
        AbsState a -> -- initial state of the program
        [Clm (AbsState a)] -> 
        [Clm (AbsState a)] -- invarianti per ogni program point
    invariantCalc inGraph ls ws is clms= 
        if next_clm == lst then clms -- trovato un pt fisso
        else (invariantCalc inGraph ls ws is (clms++[next_clm])) --sarebbe MOLTO piu' conveniente aggiungere in testa, ma e' per il senso logico
        where
            lst = (last clms)
            next_clm = updataClm inGraph ls ws is lst
   
    -- singola iterazione clm e' il risultato precedente
    -- applico la funzione nell'arco allo stato del nodo di partenza nell'iterazione precedente
    updataClm :: (AbsValueDomain a) => -- Mine' pag. 61 eq 3.3
        Adjs (AbsState a -> AbsState a) -- grafo visto con lista adiacenze in entrata
        -> [Label] -- insieme delle label nel cfg (per semplicita')
        -> W -- punti di widening
        -> AbsState a -- initial state
        -> Clm (AbsState a) -- risultato del calcolo precedente (iterazione k)
        -> Clm (AbsState a) -- iterazione k+1 (f(clm))
    updataClm in_adj labels ws is clm_k = 
        do 
            lj <- labels -- per ogni label
            if lj == L 1 then return (lj, is) -- label iniziale ad initial state
            else -- altrimenti aggiorno rispetto ad iterazione precedente
                case lookup lj in_adj of -- label in entrata a lj
                    Just in_adj_lj -> 
                            let
                                union = inUnion clm_k in_adj_lj
                            in
                                if elem lj ws then -- widening
                                    let Just xjk = lookup lj clm_k in -- stato nella Label j all'iterazione k
                                    return (lj, xjk `AS.widening` union)
                                else return (lj, union)
            

                
    inUnion :: AbsValueDomain a => 
        Clm (AbsState a)  -- risultati iterazione k per ogni program point
        -> [(Label,AbsState a -> AbsState a)] -- lista degli archi in entrata
        -> AbsState a --risultato dell'unione
    inUnion clm_k in_adj_lj = 
        foldr   (\(li,f) sr-> 
                    sr `AS.join` 
                    ( let Just xik = lookup li clm_k in  (f xik) )
                ) Bottom in_adj_lj
    -- in_adj_lj = (i,f,j) in cfg[p] (archi entranti in Lj)