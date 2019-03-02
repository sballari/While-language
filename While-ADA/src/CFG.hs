module CFG where 
    import WhileStructures
    import AbsDomain
    import AbsEval
    import AbsState as AS


    newtype Label = L Int deriving Show
    type CGraph a = [(Label,AbsState a -> AbsState a, Label)]

    freshLabel::ST (Label)
    freshLabel = ST(\s -> (L s,s+1))
    takeLabel::ST (Label)
    takeLabel = ST(\s -> (L s,s))

    cfg ::  AbsDomain a => Stm -> ST (CGraph a)
    cfg (Assign v e) = 
        do
            l1 <- freshLabel 
            l2 <- takeLabel
            return [(l1,semSG (Assign v e), l2)]
    cfg (Assert c) = 
        do
            l1 <- freshLabel
            l2 <- takeLabel
            return [(l1,condCG c, l2)]
    cfg (Skip) = 
        do  
            l1 <- freshLabel
            l2 <- takeLabel
            return [(l1,id,l2)]


    cfg (Comp s1 s2) =
        do 
            g1 <- cfg s1
            g2 <- cfg s2
            return (g1++g2)

    cfg (Cond c s1 s2) = 
        do
            l1 <- freshLabel
            l2 <- takeLabel
            g1 <- cfg s1 -- l2 -> l3
            l3 <- freshLabel
            l4 <- takeLabel
            g2 <- cfg s2 -- l4 ->l5
            l5 <- freshLabel 
            l6 <- takeLabel --join
            return ([(l1,condCG c,l2), (l1,condCG (Neg c), l4), (l3,id,l6), (l5,id,l6)]++g1++g2)
    
    cfg (While c s) = 
        do
            l1 <- freshLabel
            l2 <-freshLabel
            l3 <-takeLabel 
            g <- cfg s -- l3 -> l4
            l4 <- freshLabel
            l5 <- takeLabel
            return ([(l1,id,l2), (l2, condCG c, l3), (l2,condCG (Neg c), l5), (l4,id,l2)]++g)
           
            
            

    


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