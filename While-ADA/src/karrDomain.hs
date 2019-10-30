module KarrDomain where
    
     
    data EQs = EQs (RowForm Double,[Double]) -- <M,X> 
                 | EQsBottom deriving Show

    type RowForm a = [[a]]
    type ColForm a = [[a]] 
    
    coefs :: EQs -> RowForm Double
    coefs (EQs (m,c)) = m 
    consts :: EQs -> [Double]
    consts (EQs (m,c)) = c

    -- firstNZel :: (Eq a, Num a) => [a]->Maybe a
    -- firstNZel ([]) = Nothing 
    -- firstNZel (0:xs) = firstNZel xs
    -- firstNZel (x:xs) = (Just x)
        
    firstNZCol :: (Eq a, Num a) => ColForm a -> Maybe (a,[a])
    firstNZCol ([]) = Nothing
    firstNZCol ((0:_):cs) = firstNZCol cs
    firstNZCol ((x:xs):_) = Just (x,xs)
    

    rowM2colM :: RowForm a-> ColForm a
    rowM2colM [] = []
    rowM2colM rs = (firstCol rs):(rowM2colM (restCol rs))

    restCol :: RowForm a -> RowForm a
    --remove the first col 
    restCol ([x]:_) = []
    restCol rs = fmap tail rs

    firstCol :: RowForm a ->[a]
    firstCol = fmap head

    gaussJordanEl :: EQs -> EQs 
    gaussJordanEl (EQs ([],[])) = EQs ([],[])
    gaussJordanEl (EQs (m:ms,c:cs)) = 
        case (firstNZCol colf_ms) of
            Just (nz_el,ns_col) -> 
                let 
                    m' = fmap (/nz_el) m
                    c' = c/nz_el
                    el_coef = fmap (*(-1)) ns_col
                    ms' = zerofication ms el_coef
                    rc = gaussJordanEl (EQs (ms',cs) ) -- rec call
                in EQs (m':(coefs rc), (consts rc))

            Nothing -> 
                let rc = gaussJordanEl (EQs (ms,cs)) in -- rec call in 
                    EQs (m:(coefs rc), (consts rc))
        where
            colf_ms = rowM2colM ms
            
    zerofication ::(Num a) => RowForm a -> [a] -> RowForm a
    zerofication [] _ = []
    zerofication (m:ms) (c:cs) = (zipWith (+) (fmap (*c) m) m) : (zerofication ms cs)

