module SignDomain where
    import AbsDomain as AD
    import WhileStructures

    data Sign = Bottom | Zero | LessEqZero | MoreEqZero | Top 
                deriving Show

    instance AbsDomain Sign where
        --(<=) :: a -> a -> Bool
        (<=) Bottom _ = True
        (<=) _ Bottom = False
        
        (<=) Zero _ = True
        (<=) _ Zero = False

        (<=) LessEqZero _ = True
        (<=) _ LessEqZero = False

        (<=) LessEqZero MoreEqZero = False --Nothing, in caso di maybe bool
        (<=) MoreEqZero LessEqZero = False --Nothing, in caso di maybe bool

        (<=) MoreEqZero _ = True
        (<=) _ MoreEqZero = False

        (<=) _ Top = True
        (<=) Top _ = False
              

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
                    |(AD.<=) a b == True = b
                    |(AD.<=) a b == False = a 
                    |otherwise = Top

        

        --meet :: Sign -> Sign -> Sign
        meet x y= glb x y
            where
                glb a b
                    | (AD.<=) a b == True = a
                    | (AD.<=) a b == False = b
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
        widening x y = if (AD.<=) x y == True then x else Top -- naive