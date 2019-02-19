module AbsDomain where
    import WhileStructures
    

    class Eq a => AbsDomain a where
        (<=) :: a -> a -> Bool
        bottom :: a
        top :: a
        -- gamma :: a -> [Int] -- concretization function
        union :: a -> a -> a -- abstract version
        intersection :: a -> a -> a -- abstract version

        -- abstract operator 
        absSum :: a -> a -> a
        absMul :: a -> a -> a
        absDiv :: a -> a -> a  
        absMinus :: a -> a
        omega :: AExpr -> a
        
    

    