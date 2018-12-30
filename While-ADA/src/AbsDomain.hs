module AbsDomain where

    class Eq a => AbsDomain a where
        (<=) :: a -> a -> Bool
        bottom :: a
        top :: a
        -- gamma :: a -> [Int] -- concretization function
        union :: a -> a -> a -- abstract version
        intersection :: a -> a -> a -- abstract version
    