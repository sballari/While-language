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

    {- ##########################
    Utilities for assignment, logical elimination, and other reduction ...
    ########################## -}
    two_row_el :: 
        [Double] -> -- row a 
        [Double] -> -- row b
        Int -> -- index of column of the variable to elimanate 
        [Double] -- new constrain
    two_row_el rowa rowb i = 
        {-  
            descr : new constr with var i 0 , the col i will be 0col
            a : element in position i of row a
            b : element in position i of row b
            return : row_a + (-b/a)*row_b 
        -}
        foldr (\(e1,e2) rc -> (e1+e2):rc) [] (zip rowa mult_row_b) --rowa + mult_row_b 
        where
            a = rowa!!i
            b = rowb!!i
            mult = -(a/b)
            mult_row_b = foldr (\b rc -> (b*mult):rc ) [] rowb
        
    log_elimination :: 
        RowForm Double -> -- original matrix in row-echelon form
        Int ->  -- index of column of the variable to elimanate 
        RowForm Double -- matrix without column i, still in row-echelon form
    {-descr : algorithm of cap 5.2.3 p110 -}
    
    log_elimination (ref:rs) i = 
        foldr (\r rc-> (two_row_el r ref i):rc ) [] rs 

        


