{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module IntervalDomain where
    import AbsDomain as AD
    --import WhileStructures

    data Bound = MinInf | B Int | PlusInf deriving (Eq,Ord)
    data Interval = | Interval Bound Bound        
                    | IntervalBottom 
                    deriving (Eq)

    instance  Bound where 

    instance AbsDomain Interval where
        --top::Interval
        top = Interval MinInf PlusInf
        --bottom::Interval
        bottom = IntervalBottom

        -- <= :: Interval -> Interval -> Bool
        Interval a b <= Interval c d = a>=c && b Prelude.<= d

        IntervalBottom <= _ = True
        _ <= IntervalBottom = False

        --soundC :: Int -> Interval
        soundC n = Interval n n
        -- soundRange :: (Int ,Int) -> Interval
        soundRange (x,y) = 
                if sfasato then IntervalBottom
                else  Interval x y
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
            Interval (a+c) (b+d)       
        absSum IntervalBottom _ = IntervalBottom
        absSum _ IntervalBottom = IntervalBottom
        

        --absMul :: Interval -> Interval -> Interval
        absMul (Interval a b) (Interval c d) =
            Interval (minimum [a*c,a*d,b*c,b*d]) (maximum [a*c,a*d,b*c,b*d])
        absMul IntervalTop _ = IntervalTop
        absMul _ IntervalTop = IntervalTop
        absMul IntervalBottom _ = IntervalBottom
        absMul _ IntervalBottom = IntervalBottom
        absMul (LBound a) (Interval c d) = 
            where 
                l = if c<0 || d<0 then menoInf else minimum [a*c,a*d]
                r = if c>0 || d>0 then plusInf else maximum [a*c,a*d]



        