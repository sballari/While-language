module KarrDomain where
    
     
    data EQs = EQs (RowForm Double,[Double]) -- <M,X> 
                 | EQsBottom

    type RowForm a = [[a]]
    type ColForm a = [[a]] 
    
    coefs :: EQs -> RowForm Double
    coefs (EQs (m,c)) = m 
    consts :: EQs -> [Double]
    consts (EQs (m,c)) = c

    firstNZel :: (Eq a, Num a) => [a]->Maybe a
    firstNZel ([]) = Nothing 
    firstNZel (0:xs) = firstNZel xs
    firstNZel (x:xs) = (Just x)
        
    
    rowM2colM :: RowForm a-> ColForm a
    rowM2colM [] = []
    rowM2colM rs = (firstCol rs):(rowM2colM (restCol rs))

    restCol :: RowForm a -> RowForm a
    --remove the first col 
    restCol ([x]:_) = []
    restCol rs = fmap tail rs

    firstCol :: RowForm a ->[a]
    firstCol = fmap head


    -- gaussJordanEl :: EQs -> EQs 
    -- gaussJordanEl (EQs (m1:ms,c:cs)) =  
    --     case nz_el of 
    --         Just k ->
    --             let   
    --                 m1' = fmap (/k) m1 -- [0,..,0,1,..]
    --                 c' = c/k
                    
    --             in 
    --         Nothing -> --riga [0,0,0,...,0]
    --             gaussJordanEl (EQs (ms,cs))
        
 
    --     in 
    --         EQs (ms',cs') = gaussJordanEl (EQs (ms,cs))
    --         EQs (m':ms',c':cs')
    -- where 
    --     nz_el = firstNZel m1