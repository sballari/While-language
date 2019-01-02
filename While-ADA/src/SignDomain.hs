module SignDomain where
    import AbsDomain
    import qualified WhileStructures as WS

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
        intersection LessEq0 MoreEq0 = Eq0
        intersection Eq0 LessEq0 = Eq0
        intersection Less0 Not0 = Less0
        intersection More0 Not0 = More0
        intersection Eq0 MoreEq0 = Eq0
        intersection More0 MoreEq0 = More0   
        intersection x y = if x==y then x else union y x


        --ABSTRACT SEMANTICS
        -- absSum
        absSum Bottom _ = Bottom
        absSum _ Bottom = Bottom
        absSum Top _ = Top
        absSum _ Top = Top
        absSum x Eq0 = x
        absSum Eq0 y = y
        absSum x Not0 = Top
        absSum Not0 y = Top

        absSum Less0 Less0 = Less0
        absSum Less0 More0 = Top
        absSum Less0 LessEq0 = LessEq0
        absSum Less0 MoreEq0 = Top
        
        absSum More0 Less0 = Top
        absSum More0 More0 = More0
        absSum More0 LessEq0 = Top
        absSum More0 MoreEq0 = MoreEq0

        absSum LessEq0 LessEq0 = LessEq0
        absSum LessEq0 Less0 = Less0
        absSum LessEq0 More0 = Top
        absSum LessEq0 MoreEq0 = Top
        
        absSum MoreEq0 LessEq0 = Top 
        absSum MoreEq0 Less0 = Top
        absSum MoreEq0 More0 = MoreEq0
        absSum MoreEq0 MoreEq0 = MoreEq0

        -- absMul
        absMul Bottom _ = Bottom
        absMul _ Bottom = Bottom
        absMul _ Eq0 = Eq0
        absMul Eq0 _ = Eq0
        absMul Top _ = Top
        absMul _ Top = Top
        
        absMul _ Not0 = Not0 -- TODO pensarci
        absMul Not0 _ = Not0 -- TODO pensarci

        absMul Less0 Less0 = More0
        absMul Less0 More0 = Less0
        absMul Less0 LessEq0 = MoreEq0
        absMul Less0 MoreEq0 = LessEq0
        
        absMul More0 Less0 = Less0
        absMul More0 More0 = More0
        absMul More0 LessEq0 = LessEq0
        absMul More0 MoreEq0 = MoreEq0

        absMul LessEq0 LessEq0 = MoreEq0
        absMul LessEq0 Less0 = MoreEq0
        absMul LessEq0 More0 = LessEq0
        absMul LessEq0 MoreEq0 = LessEq0
        
        absMul MoreEq0 LessEq0 = LessEq0 
        absMul MoreEq0 Less0 = LessEq0
        absMul MoreEq0 More0 = MoreEq0
        absMul MoreEq0 MoreEq0 = MoreEq0

        -- absDiv
        absDiv Bottom _ = Bottom
        absDiv _ Bottom = Bottom
        absDiv _ Eq0 = Bottom
        absDiv Eq0 _ = Eq0
        
        absDiv Top _ = Top
        absDiv _ Top = Top
        
        absDiv _ Not0 = Not0
        absDiv Not0 _ = Not0

        absDiv Less0 Less0 = More0
        absDiv Less0 More0 = Less0
        absDiv Less0 LessEq0 = More0
        absDiv Less0 MoreEq0 = Less0
        
        absDiv More0 Less0 = Less0
        absDiv More0 More0 = More0
        absDiv More0 LessEq0 = Less0
        absDiv More0 MoreEq0 = More0

        absDiv LessEq0 LessEq0 = MoreEq0
        absDiv LessEq0 Less0 = MoreEq0
        absDiv LessEq0 More0 = LessEq0
        absDiv LessEq0 MoreEq0 = LessEq0
        
        absDiv MoreEq0 LessEq0 = LessEq0 
        absDiv MoreEq0 Less0 = LessEq0
        absDiv MoreEq0 More0 = MoreEq0
        absDiv MoreEq0 MoreEq0 = MoreEq0
        
        --absMinus 
        absMinus More0 = Less0
        absMinus MoreEq0 = LessEq0
        absMinus Less0 = More0
        absMinus LessEq0 = MoreEq0
        absMinus Eq0 = Eq0
        absMinus Not0 = Not0
        absMinus Top = Top 
        absMinus Bottom = Bottom
        


        --omega :: AExpr -> a
        omega (WS.Sum ex1 ex2) = absSum (omega ex1) (omega ex2)
        omega (WS.Mul ex1 ex2) = absMul (omega ex1) (omega ex2)
        omega (WS.Minus ex) = absMinus (omega ex)
        omega (WS.Div ex1 ex2) = absDiv (omega ex1) (omega ex2)
        omega (WS.Num x) = case signum x of 
                                        1 -> More0
                                        0 -> Eq0
                                        (-1) -> Less0
        omega (WS.Range x y) | (signum x)==1 && (signum y)==1 = More0
                                        | (signum x)==0 && (signum y)==1 = MoreEq0
                                        | (signum x)==0 && (signum y)==0 = Eq0
                                        | (signum x)==(-1) && (signum y)==(-1) = Less0 
                                        | (signum x)==0 && (signum y)==(-1) = LessEq0
                                        | (signum x)==(-1) && (signum y)==1 = Top
                                        | otherwise = omega (WS.Range y x)