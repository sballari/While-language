module AbsDomainR where
    import WhileStructures
    
    class Eq a => AbsDomainR a where
        --p63

        top :: a
        bottom :: a 
        (<=) :: a -> a -> Bool -- riflessiva, antisimmetrica, transitiva

        --soundC :: Int -> a -- sound abstraction of constant
        --soundRange :: (Int,Int) -> a -- sound abstraction of non-deterministic intervals: 
        join :: a -> a -> a -- abs lub
        meet :: a -> a -> a -- abs glb
        
        condC :: BExpr -> a -> a

        assignS :: Stm -> a -> a 
        
        
        widening :: a -> a -> a

        
