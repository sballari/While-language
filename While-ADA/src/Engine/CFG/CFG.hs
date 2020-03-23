module CFG where 
    import WhileStructures
    import AbsValueDomain
    import AbsEval
    import AbsState as AS
    import CondCFunc


    newtype Label = L Int deriving (Eq, Ord)
    type CGraph a = [(Label,AbsState a -> AbsState a, Label)] -- lista di archi del grafo computazionale
    type Graph a = [(Label, a, Label)] -- archi di un grafo generico

    freshLabel::ST (Label)
    --prendo e aumento per il prossimo
    freshLabel = ST(\s -> (L s,s+1))
    takeLabel::ST (Label)
    --prendo e la prossima volta che prento sara' la stessa
    -- 1-->2-->3 :: [(1,..,2);(2,..,3)]
    takeLabel = ST(\s -> (L s,s))

    stm2Fun :: AbsValueDomain a => Stm -> AbsState a -> AbsState a
    -- assegna la funzione semantica all'arco
    stm2Fun Skip = id
    stm2Fun (Assign x y) = semSG (Assign x y)

    -- each arc corresponds to an assignment or a condition
    createCFG :: AbsValueDomain a => CondFun a -> Stm -> ST (CGraph a)
    createCFG condC s = cfg s stm2Fun condC

    cfg ::  Stm -> (Stm -> b) -> (BExpr -> b) -> ST (Graph b) 
    cfg (Assign v e) f g = 
        do
            l1 <- freshLabel 
            l2 <- takeLabel
            return [(l1,f (Assign v e), l2)] --semSG
    cfg (Assert c) f g = 
        do
            l1 <- freshLabel
            l2 <- takeLabel
            return [(l1,g c, l2)] -- condC
    cfg (Skip) f g = 
        do  
            l1 <- freshLabel
            l2 <- takeLabel
            return [(l1,f Skip,l2)] --id


    cfg (Comp s1 s2) f g =
        do 
            g1 <- cfg s1 f g
            g2 <- cfg s2 f g
            return (g1++g2)

    cfg (Cond c s1 s2) f g = 
        do
            l1 <- freshLabel
            l2 <- takeLabel
            g1 <- cfg s1 f g -- l2 -> l3
            l3 <- freshLabel
            l4 <- takeLabel
            g2 <- cfg s2 f g -- l4 ->l5
            l5 <- freshLabel 
            l6 <- takeLabel --join
            return ([(l1,g c,l2), (l1,g (Neg c), l4), (l3,f Skip,l6), (l5,f Skip,l6)]++g1++g2)
    
    cfg (While c s) f g = 
        do
            l1 <- freshLabel
            l2 <-freshLabel
            l3 <-takeLabel 
            g1 <- cfg s f g -- l3 -> l4
            l4 <- freshLabel
            l5 <- takeLabel 
            return ([(l1,f Skip,l2), (l2, g c, l3), (l2,g (Neg c), l5), (l4,f Skip,l2)]++g1)
    

    ---------------------------------------------------
    ----------- CFG DEBUG : ARCHI PRINTABILI ----------
    ---------------------------------------------------
    debugCFG :: Stm -> ST(Graph String)
    debugCFG s = cfg s debugS debugB        
    
    debugS :: Stm -> String
    debugS s = show s
    
    debugB :: BExpr -> String 
    debugB c = show c

    ---------------------------------------------------
    -------- STATE TRANSFORMATION PATTERN CODE --------
    ---------------------------------------------------
    type State = Int
    newtype ST a = ST (State -> (a, State))

    app :: ST a -> State -> (a, State)
    app (ST f) s = f s

    instance Functor ST where
        -- fmap :: (a->b) -> ST a -> ST b
        fmap f st = ST (\s -> let (x,s') = app st s in (f x, s'))

    instance Applicative ST where
        --pure :: a -> ST a
        pure x = ST (\s -> (x,s))
        -- <*> :: ST(a->b) -> ST a -> ST b 
        (<*>) f g = ST ( \s -> let (ab,s') = app f s in let (x,s'') = app g s' in (ab x, s'') )

    instance Monad ST where 
        -- (>>=) :: ST a -> (a -> ST b) -> ST b
        (>>=) stx f = ST (\s -> let (x,s') = app stx s in app (f x) s' )

    ---------------------------------------------------
    ------ STATE TRANSFORMATION PATTERN CODE END ------
    ---------------------------------------------------

    ---------------------------------------------------
    -------- PRINT PROGRAM WITH LABEL CODE END --------
    ---------------------------------------------------

    printLabProg :: [String] -> String 
    -- stampa il programma labellato
    printLabProg = foldr (\ln r -> ln++"\n"++r) []

    addToEnd :: [String] -> String -> [String]
    addToEnd [] str = [str]
    addToEnd (x:[]) str = [x++str]
    addToEnd (x:xs) str = x:(addToEnd xs str)

    shiftRight1Tab :: [String] -> [String]
    shiftRight1Tab = foldr (\x sr -> ("\t"++x) : sr ) []
    
    labelled :: Stm -> ST([String])
    labelled prg =
        do 
            lc <- labelledCode prg
            labf <- takeLabel
            return (addToEnd lc ("\n["++show(labf)++"]"))
    
    labelledCode :: Stm -> ST ([String])
    labelledCode (Assign v e) = 
        do
            l1 <- freshLabel
            return ["["++(show l1)++"] "++v++":= "++show(e)]
    
    
    labelledCode (Assert c) = 
        do
            l1 <- freshLabel
            return ["["++(show l1)++"] assert"++ (show c)]

    
    labelledCode (Skip) = 
        do  
            l1 <- freshLabel
            return ["["++(show l1)++"] Skip"]
        

    labelledCode (Comp s1 s2) =
        do 
            p1 <- labelledCode s1
            p2 <- labelledCode s2
            return ((addToEnd p1 ";")++p2)


    labelledCode (Cond c s1 s2) = 
        do
            l1 <- freshLabel
            
            g1 <- labelledCode s1 -- l2 -> l3
            l3 <- freshLabel
            
            g2 <- labelledCode s2 -- l4 ->l5
            l5 <- freshLabel 
             --join
            return (["["++(show l1)++"] if "++ (show c) ++" then "] 
                    ++ ["("] 
                    ++ g1 
                    ++ ["["++(show l3)++"])","else","("]   
                    ++g2
                    ++["["++(show l5)++"])"])
    
    labelledCode (While c s) = 
        do
            l1 <- freshLabel
            l2 <-freshLabel
             
            g1 <- labelledCode s -- l3 -> l4
            l4 <- freshLabel
            
            return (["["++(show l1)++"] while ["++(show l2)++"] "++ (show c) ++" do "]
                    ++ ["("] 
                    ++ (shiftRight1Tab g1)  
                    ++ ["["++(show l4)++"] )"])
    
    ---------------------------------------------------
    ------------ GRAPH UTILITY FUNCTIONS  -------------
    ---------------------------------------------------
    type Adjs a = [(Label,[(Label,a)])]
    -- Grafo visto come lista delle liste di adiacenza in entrata (convertitore)

    labels :: Graph a -> [Label]
    labels cfg = [L i|i <- [1..m]]
        where L m = maximum (concat [[li,lj]|(li,_,lj)<-cfg])
    
    -- prende in input un cfg e ritorna il grafo visto come una lista delle adiacenze in entrata.
    in_adjs :: Graph a -> Adjs a
    in_adjs cfg = [  (lj,[(li,f)|(li,f,lk)<-cfg, lj == lk])   | lj <-(labels cfg)] -- O(m+n*m): se denso n^3


    ---------------------------------------------------
    ------------ GRAPH print FUNCTIONS  -------------
    ---------------------------------------------------
    
    pedix :: Int -> String
    pedix 0 = "\8320" 
    pedix 1 = "\8321"
    pedix 2 = "\8322"
    pedix 3 = "\8323"
    pedix 4 = "\8324"
    pedix 5 = "\8325"
    pedix 6 = "\8326"
    pedix 7 = "\8327"
    pedix 8 = "\8328" 
    pedix 9 = "\8329" 

    pedix n = (pedix firsts) ++ (pedix last)
        where 
            firsts = (n `div` 10)
            last = (n `mod` 10)

    instance Show Label where
        show (L n) = '\8467':(pedix n)