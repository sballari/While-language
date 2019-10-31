module KarrDomain where
    
     
    data EQs = EQs (RowForm Double,[Double]) -- <M,X> 
                 | EQsBottom deriving (Show, Eq)

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
        
    firstNZRow :: (Eq a, Num a) => RowForm a -> Maybe (a,[a])
    -- nonzero element
    -- sub Row
    firstNZRow ([]) = Nothing
    firstNZRow ((0:_):cs) = firstNZRow cs
    firstNZRow ((x:xs):_) = Just (x,xs)
    
    firstNZCol :: (Eq a, Num a) => RowForm a -> Maybe (a,[a])
    firstNZCol = firstNZRow.rowM2colM

    rowM2colM :: RowForm a-> ColForm a
    rowM2colM [] = []
    rowM2colM rs = (firstCol rs):(rowM2colM (restCol rs))

    transpose = rowM2colM

    restCol :: RowForm a -> RowForm a
    --remove the first col 
    restCol ([x]:_) = []
    restCol rs = fmap tail rs

    firstCol :: RowForm a ->[a]
    firstCol = fmap head

    gaussJordanEl :: EQs -> EQs 
    gaussJordanEl (EQs ([[]],[])) = EQs ([[]],[])
    gaussJordanEl (EQs (m:ms,c:cs)) = 
        case (firstNZCol ms) of
            Just (nz_el,nz_col) -> 
                let 
                    m' = fmap (/nz_el) m
                    c' = c/nz_el
                    el_coef = fmap (*(-1)) nz_col
                    ms' = zerofication ms m' el_coef
                    [cs'] = transpose (zerofication (transpose [cs]) [c'] el_coef )
                    --TODO: si potrebbe scrivere una zerofication ad hoc senza dover fare la trasporta
                    rc = gaussJordanEl (EQs (ms',cs') ) -- rec call
                in EQs (m':(coefs rc), c':(consts rc))

            Nothing -> 
                let rc = gaussJordanEl (EQs (ms,cs)) in -- rec call 
                    EQs (m:(coefs rc), c:(consts rc))

            
    zerofication ::(Num a) => RowForm a -> [a] -> [a] -> RowForm a
    -- sottomatrice da zeroficare
    -- riga regina 
    -- coefficiente di eliminazione 
    zerofication [] _ _ = []
    zerofication ([[]]) _ _ = []
    zerofication (m:ms) qr (ec:ecs) = (zipWith (+) (fmap (*ec) qr) m) : (zerofication ms qr ecs)

    