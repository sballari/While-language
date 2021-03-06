module AbsDomain where
    import WhileStructures
    
    class Eq a => AbsDomain a where
        --this is the Value abstract domain

        top :: a
        bottom :: a 
        (<=) :: a -> a -> Bool -- riflessiva, antisimmetrica, transitiva

        soundC :: Int -> a -- sound abstraction of constant
        soundRange :: (Int,Int) -> a -- sound abstraction of non-deterministic intervals: 
        join :: a -> a -> a -- abs lub
        meet :: a -> a -> a -- abs glb
        

        -- sound binary operators 
        absSum :: a -> a -> a
        absMul :: a -> a -> a               
        absDiv :: a -> a -> a  
        -- sound unary operators
        absMinus :: a -> a
        
        widening :: a -> a -> a

        
