--{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module KarrDomain where
    
    data EQs = EQs (RowForm Double,[Double]) -- <M,X> 
                 | EQsBottom deriving (Show, Eq)

    type RowForm a = [[a]]
    type ColForm a = [[a]] 
    
    coefs :: EQs -> RowForm Double
    coefs (EQs s)=fst s
    consts :: EQs -> [Double]
    consts (EQs s)=snd s

        
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

    gaussJordanEl :: EQs -> EQs 

    gaussJordanEl (EQs ([],[])) = EQs ([],[]) --maybe useless
    gaussJordanEl (EQs (m:[],[c])) = 
        case firstNZCol ([m]) of
            Just (nz_el,nz_col) -> EQs ([fmap (/nz_el) m], [c/nz_el])
            Nothing -> EQs ([m],[c])

    gaussJordanEl (EQs (m:ms,c:cs)) = 
        case (firstNZCol (m:ms)) of 
            Just (nz_el,nz_col) -> 
                let 
                    m' = fmap (/nz_el) m  --row queen
                    c' = c/nz_el -- c queen
                    el_coef = fmap (*(-1)) nz_col -- coeffs di eliminazione
                    ms' = zerofication ms m' el_coef
                    [cs'] = transpose (zerofication (transpose [cs]) [c'] el_coef ) --actung!
                    --TODO: si potrebbe scrivere una zerofication ad hoc senza dover fare la trasporta
                    rc = gaussJordanEl (EQs (ms',cs') ) -- rec call
                in EQs (m':(coefs rc), c':(consts rc))

            Nothing -> 
                let rc = gaussJordanEl (EQs (ms,cs)) in -- rec call 
                    EQs (m:(coefs rc), c:(consts rc))

            
    zerofication ::(Num a,Show a) => RowForm a -> [a] -> [a] -> RowForm a
    -- sottomatrice da zeroficare
    -- riga regina 
    -- coefficiente di eliminazione 
    zerofication [] _ _ = []
    zerofication (m:ms) qr (ec:ecs) = (zipWith (+) (fmap (*ec) qr) m) : (zerofication ms qr ecs)

    main  = 
        let
            m = [[0,1,2,1],[0,0,1,2]]
            c = [3,3]
        in print (gaussJordanEl (EQs (m,c)))
        