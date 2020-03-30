module MatrixUtilities where
    import qualified Data.List as L
    
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
        
    log_elimination :: --VAR OUT OF BASE (NON LEADING POSITION!!!!)
        RowForm Double -> -- original matrix in row-echelon form
        Int ->  -- index of column of the NON LEADING variable to elimanate 
        RowForm Double -- matrix without column i, still in row-echelon form
    {-descr : logical elimination of variable in 
            column i ,algorithm of cap 5.2.3 p110 -}
    log_elimination (ref:[]) i = 
        if ref!!i==0
        then (ref:[]) else []  --TODO : pensarci meglio [X+Y=2] [1,0|2] s
    log_elimination (ref:rs) i = 
        foldl (\rc r-> (two_row_el r ref i):rc ) [] rs 

        

    lead_check :: 
        [Double] -> --column
        Bool -- returns true iff the coeff are all one exept for one 1
    lead_check l =  
        if r  == (1,True) then True else  False
        where 
        r= foldr (\c (sum,j01) ->
                    if j01 then 
                        case c of 
                            1 -> (sum+1,True)
                            0 -> (sum, True)
                            _ -> (0,False)
                    else (0,False)
                    ) (0,True) l

    inversion_coefficients ::   -- (invertible case)
        ([Double],Double) ->    -- assign expr e,  vj<-e
        Int ->                  -- index of vj
        Double ->               -- a_vj : coef of vj in e
        ([Double],Double)       -- inversion coefficients for the log elimination of a outbase var
    inversion_coefficients (coefficients,b) var_index a_vj = ((
                                    foldr (
                                        \(a,i) rc -> 
                                            if i==var_index then (1/a_vj):rc
                                            else (-a/a_vj):rc
                                        ) [] (zip coefficients [0..])
                                ), -b/a_vj)

    zerosWith1NZelement :: --3 1 66 -> [0,66,0]
        Int -> --lenght of the vector of zeros
        Int -> -- position of non zero element
        Double -> -- element non zero
        [Double] 
    zerosWith1NZelement len ind el = 
        foldr (\i rc -> if i==ind then (el:rc) else (0:rc)) [] [0..(len-1)]

    leading_matrix_info :: 
        RowForm Double -> -- system in row-echelon form
        Int ->            -- number of varibles in the sys
        [(Bool,Int,Int)]  -- foreach col : is_leading or not , index of the col, index of the row where the coeff isn't 0 
        {-
        descr : returns (check ,col_index,index of NZ element) of leading column
        -}
    leading_matrix_info join_system varN = foldr 
                                (\(column,col_index) rc -> 
                                    let 
                                        check = lead_check column
                                        Just o_i = if check then L.findIndex (==1) column else (Just (-1))
                                    in 
                                    (check,col_index,o_i):rc
                                ) [] (zip (transpose join_system) [0..])