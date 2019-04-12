{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module IntervalDomain where
    import AbsDomain as AD
    --import WhileStructures

    data Interval = RBound Int           
                    | Interval Int Int  
                    | LBound Int          
                    | IntervalTop          
                    | IntervalBottom 
                    deriving (Eq)

    instance AbsDomain Interval where
        --top::Interval
        top = IntervalTop
        --bottom::Interval
        bottom = IntervalBottom

        -- <= :: Interval -> Interval -> Bool
        _ <= IntervalTop = True
        IntervalTop <= _ = False
        Interval a b <= Interval c d = a>=c && b Prelude.<= d

        RBound a <= RBound b = b>=a
        RBound a <= _ = False 

        Interval _ r <= RBound x = r Prelude.<=x
        _ <= RBound x = False

        LBound l <= LBound y = l >= y 
        LBound l <= _ = False

        Interval x y <= LBound l = x >= l 
        _ <= LBound l = False

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
        join IntervalTop _ = IntervalTop
        join _ IntervalTop = IntervalTop
        join IntervalBottom x = x
        join x IntervalBottom = x
        join (LBound x) (Interval a b) = LBound (min x a)
        join (Interval a b) (LBound x) = LBound (min x a)
        join (LBound x) (LBound y) = LBound (min x y)
        join (RBound x) (RBound y) = RBound (max x y)
        join (LBound _) (RBound _) = IntervalTop
        join (RBound _) (LBound _) = IntervalTop
        join (RBound r) (Interval a b) = RBound (max r b)
        join (Interval a b) (RBound r) = RBound (max r b)



        --meet :: Interval -> Interval -> Iterval
        meet (Interval a b) (Interval c d) = 
            if max a c Prelude.<= min b d then
                Interval (max a c) (min b d)
            else IntervalBottom 

        meet IntervalTop x = x
        meet x IntervalTop = x
        meet IntervalBottom _ = IntervalBottom
        meet _ IntervalBottom = IntervalBottom
        meet (LBound x) (Interval a b) = 
            if  b Prelude.<= x then IntervalBottom
            else Interval (max a x) b
        meet (Interval a b) (LBound x) = 
            if  b Prelude.<= x then IntervalBottom
            else Interval (max a x) b
        meet (LBound x) (LBound y) = LBound (max x y)
        meet (RBound x) (RBound y) = RBound (min x y)
        meet (LBound x) (RBound y) = 
            if y < x then IntervalBottom 
            else Interval x y
        meet (RBound y) (LBound x) = 
            if y < x then IntervalBottom 
            else Interval x y
        meet (RBound r) (Interval a b) = 
            if a Prelude.<= r then 
                Interval a (min r b)
            else IntervalBottom
        meet (Interval a b) (RBound r) = 
            if a Prelude.<= r then 
                Interval a (min r b)
            else IntervalBottom

        --absSum :: Interval -> Interval -> Interval
        absSum (Interval a b) (Interval c d) =
            Interval (a+c) (b+d)
        absSum IntervalTop _ = IntervalTop
        absSum _ IntervalTop = IntervalTop
        absSum IntervalBottom _ = IntervalBottom
        absSum _ IntervalBottom = IntervalBottom
        absSum (LBound x) (Interval a b) = LBound (x+a)
        absSum (Interval a b) (LBound x) = LBound (x+a)
        absSum (LBound _) (RBound _) = IntervalTop
        absSum (RBound _) (LBound _) = IntervalTop
        absSum (LBound x) (LBound y) = LBound (x+y)
        absSum (RBound x) (RBound y) = RBound (x+y)
        absSum (RBound x) (Interval a b) = RBound (x+a)
        absSum (Interval a b) (RBound x) = RBound (x+a)


        --absMul :: Interval -> Interval -> Interval
        absMul (Interval a b) (Interval c d) =
            Interval (minimum [a*c,a*d,b*c,b*d]) (maximum [a*c,a*d,b*c,b*d])
        -- absMul IntervalTop _ = IntervalTop
        -- absMul _ IntervalTop = IntervalTop
        -- absMul IntervalBottom _ = IntervalBottom
        -- absMul _ IntervalBottom = IntervalBottom
        -- absMul (LBound x) (Interval a b) = LBound (x+a)
        -- absMul (Interval a b) (LBound x) = LBound (x+a)
        -- absMul (LBound _) (RBound _) = IntervalTop
        -- absMul (RBound _) (LBound _) = IntervalTop
        -- absMul (LBound x) (LBound y) = LBound (x+y)
        -- absMul (RBound x) (RBound y) = RBound (x+y)
        -- absMul (RBound x) (Interval a b) = RBound (x+a)
        -- absMul (Interval a b) (RBound x) = RBound (x+a)


        