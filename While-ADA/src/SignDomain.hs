module SignDomain where
    import AbsDomain as AD
    import WhileStructures

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
            |n Prelude.== 0 = Zero
            |n Prelude.>= 0 = MoreEqZero
            |n Prelude.<= 0 = LessEqZero
        
        alfa (Range n n')
            |n Prelude.== 0 = MoreEqZero
            |n Prelude.<= 0 && n' Prelude.<= 0 = LessEqZero
            |n Prelude.<= 0 && n' Prelude.>= 0 = Top
            |n Prelude.>= 0 && n' Prelude.>= 0 = MoreEqZero
            |n Prelude.>= 0 && n' Prelude.<= 0 = Bottom
        
        --join :: Sign -> Sign -> Sign

        join x y = lub x y
            where 
                lub a b  
                    |(AD.<=) a b == Just True = b
                    |(AD.<=) a b == Just False = a 
                    |otherwise = Top

        

        --meet :: Sign -> Sign -> Sign
        meet x y= glb x y
            where
                glb a b
                    | (AD.<=) a b == Just True = a
                    | (AD.<=) a b == Just False = b
                    | otherwise = Bottom

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


        {-
        absMul :: a -> a -> a               
        absDiv :: a -> a -> a  
        absMinus :: a -> a  
        -}
        
        -- widening :: Sign -> Sign -> Sign
        widening x y = if (AD.<=) x y == Just True then x else Top -- naive