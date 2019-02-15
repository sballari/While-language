module AbsDomain where
    import WhileStructures
    
    
    class AbsDomain a where
        -- (<=) :: a -> a -> Bool
        (<=) :: a -> a -> Maybe Bool -- ordine parziale
        top :: a
        bottom :: a 
        --gamma :: a -> [Int] -- concretization function : B# -> power(I)
        alfa :: AExpr -> a  -- astrae costanti (c) e intervalli non deterministici ([c,c'])
        join :: a -> a -> a -- lub
        meet :: a -> a -> a -- glb
        
        -- unary operators
        absNeg :: a -> a

        -- binary operators 
        absSum :: a -> a -> a
        absMul :: a -> a -> a               
        absDiv :: a -> a -> a  
        absMinus :: a -> a
        
        widening :: a -> a -> a

        -- omega :: AExpr -> a       3 + 2  omega (Plus (N 3) (N 2)) = absSum (alfa (N 3)) (alfa (N 4))