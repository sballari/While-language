module AbsDomain where

    class Eq a => AbsDomain a where
        (<=) :: a -> a -> Bool
        bottom :: a
        top :: a
        -- gamma :: a -> [Int] -- concretization function
        union :: a -> a -> a
        intersection :: a -> a -> a
    
    -- SIGN DOMAIN
    data Sign = Bottom | Top | 
                LessEq0 | Less0 |
                Eq0 | More0 | MoreEq0 |
                Not0 deriving (Eq, Show)
    
    instance AbsDomain Sign where
        
        (<=) Bottom _ = True
        (<=) _ Top = True
        (<=) Top _ = False
        (<=) _ Bottom = False
        (<=) Less0 LessEq0 = True
        (<=) Eq0 LessEq0 = True
        (<=) Less0 Not0 = True
        (<=) More0 Not0 = True
        (<=) Eq0 MoreEq0 = True
        (<=) More0 MoreEq0 = True
        (<=) x y = if x==y then True else False
    
        bottom = Bottom
        top = Top

        
        union Bottom y = y
        union _ Top = Top
        union Less0 LessEq0 = LessEq0
        union LessEq0 More0 = Top
        union Eq0 LessEq0 = LessEq0
        union Less0 Not0 = Top
        union More0 Not0 = Top
        union Eq0 MoreEq0 = MoreEq0
        union More0 MoreEq0 = MoreEq0   
        union x y = if x==y then x else union y x

        intersection Bottom y = Bottom
        intersection x Top = x
        intersection Less0 LessEq0 = Less0
        intersection LessEq0 More0 = Bottom
        intersection Eq0 LessEq0 = Eq0
        intersection Less0 Not0 = Less0
        intersection More0 Not0 = More0
        intersection Eq0 MoreEq0 = Eq0
        intersection More0 MoreEq0 = Eq0   
        intersection x y = if x==y then x else union y x