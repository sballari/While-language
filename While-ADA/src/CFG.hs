module CFG where 
    import WhileStructures
    import AbsDomain
    import AbsEval
    import AbsState as AS
    import CondCFunc


    newtype Label = L Int deriving (Show, Eq, Ord)
    type CGraph a = [(Label,AbsState a -> AbsState a, Label)]
    type Graph a = [(Label, a, Label)]


    freshLabel::ST (Label)
    freshLabel = ST(\s -> (L s,s+1))
    takeLabel::ST (Label)
    takeLabel = ST(\s -> (L s,s))

    createCFG :: AbsDomain a => CondFun a -> Stm -> ST (CGraph a)
    createCFG condC s = cfg s convertS condC

    debugCFG :: Stm -> ST(Graph String)
    debugCFG s = cfg s debugS debugB

    cfg ::  Stm -> (Stm -> b) -> (BExpr -> b) -> ST (Graph b)
    cfg (Assign v e) f g= 
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
            g1 <- cfg s1 f g-- l2 -> l3
            l3 <- freshLabel
            l4 <- takeLabel
            g2 <- cfg s2 f g-- l4 ->l5
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
            l5 <- takeLabel --WARNING
            return ([(l1,f Skip,l2), (l2, g c, l3), (l2,g (Neg c), l5), (l4,f Skip,l2)]++g1)
           
    convertS :: AbsDomain a => Stm -> AbsState a -> AbsState a
    convertS Skip = id
    convertS (Assign x y) = semSG (Assign x y)
    
            
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

    addToEnd [] str = [str]
    addToEnd (x:xs) str = x:(addToEnd xs str)
    
    labelledCode:: Stm -> ST ([String])

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
                    ++ [")","else","("]   
                    ++g2
                    ++[")"])
    
    labelledCode (While c s) = 
        do
            l1 <- freshLabel
            l2 <-freshLabel
             
            g1 <- labelledCode s -- l3 -> l4
            l4 <- freshLabel
            L5 <- takeLabel
            
            return (["["++(show l1)++"] while ["++(show l2)++"] "++ (show c) ++" do "]
                    ++ ["("] 
                    ++ g1  
                    ++ ["["++(show l4)++"] ) ","["++(show l5)++"]"]   )
    