--{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module KarrDomain where
    import AbsDomainR
    import MatrixUtilities
    
    data EQs = EQs (RowForm Double,[Double]) -- <M,X> in a row-echelon form
                 | EQsBottom deriving (Show, Eq)
    
    instance AbsDomainR EQs where 
        -- top :: EQs
        top = EQs ([],[])
        -- bottom :: EQs
        bottom = EQsBottom
        -- (<=) :: EQs -> EQs -> Bool
        -- A C= B <-> A meet B = A p.108
        EQs (m1,c1) <= EQs (m2,c2) = (EQs (m1,c1) `meet` EQs (m2,c2)) == EQs (m1,c1) 
        --NOTE: this comparision has sense iff both the side are in a rowEchelon form

        {-
        explicit_join :: EQs -> EQs -> EQs -- lub
        EQs (m1,c1) `join` EQs (m2,c2) = 
            foldr (\rm1 sr -> rm1++()) [] m1
            where 
                varN = length (m1 !! 0) -- cardinality of V
                
        -}


        -- meet :: EQs -> EQs -> EQs 
        EQs (m1,c1) `meet` EQs (m2,c2) = EQs (rowEchelonForm (m1++m2, c1++c2))

    