module SignDomain where
    import AbsDomain

    data Sign = Bottom | Zero | LessEqZero | MoreEqZero | Top 
                deriving Show

    instance AbsDomain Sign where
        --(<=) :: a -> a -> Maybe Bool
        (<=) Bottom _ = Just True
        (<=) _ Bottom = Just False
        
        (<=) Zero _ = Just True
        (<=) _ Zero = Just False

        (<=) LessEqZero _ = Just True
        (<=) _ LessEqZero = Just False

        (<=) LessEqZero MoreEqZero = Nothing
        (<=) MoreEqZero LessEqZero = Nothing

        (<=) MoreEqZero _ = Just True
        (<=) _ MoreEqZero = Just False

        (<=) _ Top = Just True
        (<=) Top _ = Just False
              

        -- bottom :: a
        bottom = Bottom

        -- top :: a
        top = Top
        
        -- gamma :: a -> [Int]
        -- gamma Bottom = []
        -- gamma Zero = [0]
        -- gamma LessEqZero = [0,-1..]
        -- gamma MoreEqZero = [0..]
        -- gamma Top = (reverse [0,-1..]) ++ [1..]

        -- alfa :: AExpr -> a
        alfa (Num n)
            n == 0 = Zero
            n >= 0 = MoreEqZero
            n <= 0 = LessEqZero
        
        alfa (Range n n')
            n == 0 = MoreEqZero
            n <= 0 & n' <= 0 = LessEqZero
            n <= 0 & n' >= 0 = Top
            n >= 0 & n' >= 0 = MoreEqZero
            n >= 0 & n' <= 0 = Bottom
        
        --join :: a -> a -> a

        lub :: a -> a -> abs
        lub x y 
            (x <= y) == Just True = y
            (x <= y) == Just False = x 
            otherwise = Top

        join x y = lub x y

        --meet :: a -> a -> a

        glb :: a -> a -> a
        glb x y
            (x <= y) == Just True = x
            (x <= y) == Just False = y
            otherwise = Bottom

        -- unary operators --> necessario?
        --absNeg :: a -> a
        absNeg Bottom = Bottom
        absNeg Zero = Zero
        absNeg LessEqZero = MoreEqZero
        absNeg MoreEqZero = LessEqZero
        absNeg Top = Top

        -- binary operators 
        --absSum :: a -> a -> a
        absSum Bottom _ = Bottom


        absMul :: a -> a -> a               
        absDiv :: a -> a -> a  
        absMinus :: a -> a
        
        widening :: a -> a -> a
        widening x y = if x <= y then x else Top -- naive