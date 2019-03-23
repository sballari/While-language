module AbsDomain where
    import WhileStructures
    
    class Eq a => AbsDomain a where

        top :: a
        bottom :: a 
        (<=) :: a -> a -> Bool

        --gamma :: a -> [Int] -- concretization function : B# -> power(I)
        --alfa :: AExpr -> a  --non obbligatoria

        soundC :: Int -> a -- obbligatoria
        soundRange :: (Int,Int) -> a -- obbligatoria
        join :: a -> a -> a -- abs lub
        meet :: a -> a -> a -- abs glb
        

        -- sound binary operators 
        absSum :: a -> a -> a
        absMul :: a -> a -> a               
        absDiv :: a -> a -> a  
        -- sound unary operators
        absMinus :: a -> a
        
        widening :: a -> a -> a

        

