--{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module IntervalDomain where
    import AbsValueDomain as AD
    import Prelude hiding ((/),(*),negate,(+),(-))
    import qualified Prelude as P ((*),negate,(+),(-))

    data Bound = MinInf | B Int | PlusInf deriving (Eq,Ord,Read)
    data Interval = Interval Bound Bound        
                    | IntervalBottom 
                    deriving (Eq,Read)

    instance Show Bound where 
        show MinInf = '-':"\8734"
        show PlusInf = '+':"\8734"
        show (B n) = show n

    instance Show Interval where 
        show IntervalBottom = "\8869"++"int"
        show (Interval MinInf PlusInf) = "\8868"++"int"
        show (Interval b1 b2) = "["++ (show b1) ++","++ (show b2) ++"]"

    (+) :: Bound -> Bound ->Bound
    (B x) + (B y) = B (x P.+ y) 
    (MinInf) + (B _) = MinInf
    (B _ ) + (MinInf) = MinInf
    (PlusInf) + (B _) = PlusInf
    (B _ ) + (PlusInf) = PlusInf
    (MinInf) + (MinInf) = MinInf
    (PlusInf) + (PlusInf) = PlusInf 

    (-) :: Bound -> Bound ->Bound
    x - y = x + (negate y)

    (*) :: Bound -> Bound -> Bound
    (B x) * (B y) = B (x P.* y)   
    (MinInf) * (B c) = 
        if c == 0 then B 0 
        else if c<0 then PlusInf
        else MinInf
    (B c ) * (MinInf) = 
        if c == 0 then B 0 
        else if c<0 then PlusInf
        else MinInf
    (PlusInf) * (B c) = 
        if c == 0 then B 0 
        else if c<0 then MinInf
        else PlusInf
    (B c) * (PlusInf) = 
        if c == 0 then B 0 
        else if c<0 then MinInf
        else PlusInf
    (MinInf) * (MinInf) = PlusInf
    (PlusInf) * (PlusInf) = PlusInf
    (PlusInf) * (MinInf) = MinInf 
    (MinInf) * (PlusInf) = MinInf
    
    negate :: Bound -> Bound
    negate MinInf = PlusInf
    negate PlusInf = MinInf 
    negate (B c) = B (P.negate c)
 
    (/) :: Bound -> Bound -> Bound
    -- divisione esatta
    (B x) / (B y) = B (x `div` y)
    (MinInf) / (B c) = 
        if c>0 then MinInf 
        else 
            if c<0 then PlusInf
            else error "Division by zero"
    
    (B _ ) / (MinInf) = B 0

    (PlusInf) / (B c) = 
        if c>0 then PlusInf 
        else 
            if c<0 then MinInf
            else error "Division by zero"
        
    (B _ ) / (PlusInf) = B 0
    (MinInf) / (MinInf) = B 0
    (PlusInf) / (PlusInf) = B 0
    (PlusInf) / (MinInf) = B 0 
    (MinInf) / (PlusInf) = B 0 
     
 
    instance AbsValueDomain Interval where
        --top::Interval
        top = Interval MinInf PlusInf
        --bottom::Interval
        bottom = IntervalBottom

        -- <= :: Interval -> Interval -> Bool
        Interval a b <= Interval c d = a>=c && b Prelude.<= d

        IntervalBottom <= _ = True
        _ <= IntervalBottom = False

        --soundC :: Int -> Interval
        soundC n = Interval (B n) (B n)
        -- soundRange :: (Int ,Int) -> Interval
        soundRange (x,y) = 
                if sfasato then IntervalBottom
                else  Interval (B x) (B y)
            where 
                sfasato = x > y

        --join :: Interval -> Interval -> Iterval
        join (Interval a b) (Interval c d) = 
            Interval (min a c) (max b d)
        join IntervalBottom x = x
        join x IntervalBottom = x
        

        --meet :: Interval -> Interval -> Iterval
        meet (Interval a b) (Interval c d) = 
            if max a c Prelude.<= min b d then
                Interval (max a c) (min b d)
            else IntervalBottom 
        meet IntervalBottom _ = IntervalBottom
        meet _ IntervalBottom = IntervalBottom
        

        --absSum :: Interval -> Interval -> Interval
        absSum (Interval a b) (Interval c d) =
            Interval (a + c ) (b + d)       
        absSum IntervalBottom _ = IntervalBottom
        absSum _ IntervalBottom = IntervalBottom
        

        --absMul :: Interval -> Interval -> Interval
        absMul (Interval a b) (Interval c d) =
            Interval (minimum [a*c,a*d,b*c,b*d]) (maximum [a*c,a*d,b*c,b*d])
        absMul IntervalBottom _ = IntervalBottom
        absMul _ IntervalBottom = IntervalBottom

        --absDiv :: Interval -> Interval -> Interval
        absDiv _ IntervalBottom = IntervalBottom
        absDiv (Interval a b) (Interval c d) 
        -- nel caso di divisione per 0: x y sono intersezioni vuote
            | (B 1) Prelude.<= c = Interval (Prelude.minimum [a/c,a/d]) (Prelude.maximum [b/c,b/d])
            | d Prelude.<= (B (-1)) = Interval (Prelude.minimum [b / c, b / d] ) (Prelude.maximum [a/c,a/d])
            | otherwise =   let 
                                -- nel caso di divisione per 0: x y sono intersezioni vuote
                                x = meet (Interval c d) (Interval (B 1) PlusInf) 
                                y = meet (Interval c d) (Interval MinInf (B (-1)))
                                p1 = (Interval a b) `absDiv` x
                                p2 = (Interval a b) `absDiv` y
                            in join p1 p2

        --absMin::Interval -> Interval
        absMinus (Interval a b) = Interval (negate b) (negate a)
        absMinus (IntervalBottom) = IntervalBottom

        --widening :: a -> a -> a
        widening (Interval a b) (Interval c d) = 
            Interval l r
            where 
                l = if a Prelude.<= c then a else MinInf
                r = if b >= d then b else PlusInf 