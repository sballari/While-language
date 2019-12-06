module MatrixUtilities where
    type RowForm a = [[a]]
    type ColForm a = [[a]] 

    zeros :: (Num a) => Int -> [a]
    zeros n = replicate n 0

    ones :: (Num a) => Int -> [a]
    ones n = replicate n 1

    coefs :: (RowForm Double,[Double],[String]) -> RowForm Double
    coefs (a,b,c) = a  
    consts :: (RowForm Double,[Double],[String]) -> [Double]
    consts (a,b,c)= b 

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
    firstCol ms = fmap head ms

    gaussJordanEl :: (RowForm Double,[Double],[String]) -> (RowForm Double,[Double],[String]) 
    gaussJordanEl ([],[],[]) = ([],[],[]) --maybe useless
    gaussJordanEl (m:[],[c],o) = 
        case firstNZCol ([m]) of
            Just (nz_el,nz_col) -> ([fmap (/nz_el) m], [c/nz_el],o)
            Nothing -> ([m],[c],o)

    gaussJordanEl (m:ms,c:cs,o) = 
        case (firstNZCol (m:ms)) of 
            Just (nz_el,nz_col) -> 
                let 
                    m' = fmap (/nz_el) m  --row queen
                    c' = c/nz_el -- c queen
                    el_coef = fmap (*(-1)) nz_col -- coeffs di eliminazione
                    ms' = zerofication ms m' el_coef
                    [cs'] = transpose (zerofication (transpose [cs]) [c'] el_coef ) --actung!
                    --TODO: si potrebbe scrivere una zerofication ad hoc senza dover fare la trasporta
                    rc = gaussJordanEl (ms',cs',o) -- rec call
                in (m':(coefs rc), c':(consts rc),o)

            Nothing -> 
                let rc = gaussJordanEl (ms,cs,o) in -- rec call 
                    (m:(coefs rc), c:(consts rc),o)


    zerofication ::(Num a) => RowForm a -> [a] -> [a] -> RowForm a
    -- sottomatrice da zeroficare
    -- riga regina 
    -- coefficiente di eliminazione 
    zerofication [] _ _ = []
    zerofication (m:ms) qr (ec:ecs) = (zipWith (+) (fmap (*ec) qr) m) : (zerofication ms qr ecs)

    rowEchelonForm :: (RowForm Double,[Double],[String]) -> (RowForm Double,[Double],[String])
    rowEchelonForm eqs = 
        let 
            (m,c,o) = gaussJordanEl eqs
            (m',c',o') = gaussJordanEl (reverse m, reverse c, reverse o)
        in (reverse m', reverse c', reverse o') -- in realta' non servirebbe fare la reverse. Serve o non serve! Non confondermi Mona!