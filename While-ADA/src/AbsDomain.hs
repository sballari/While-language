module AbsDomain where
    import WhileStructures
    

    class Eq a => AbsDomain a where
        (<=) :: a -> a -> Bool
        (<) :: a -> a -> Bool
        (>=) :: a -> a -> Bool
        (>) :: a -> a -> Bool

        (<) a b = a AbsDomain.<= b && a/=b
        (>=) a b = if (a AbsDomain.< b) then Prelude.False else Prelude.True
        (>) a b = a AbsDomain.>= b && a /= b

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

        
        
    

    