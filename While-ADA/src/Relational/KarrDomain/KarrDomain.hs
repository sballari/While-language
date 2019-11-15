--{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module KarrDomain where
    import AbsDomainR
    import MatrixUtilities
    
    data EQs = EQs (RowForm Double,[Double]) -- <M,X> in a row-echelon form
                 | EQsBottom deriving (Show, Eq)

    applyST :: ((RowForm Double,[Double])-> (RowForm Double,[Double])) -> EQs -> EQs
    -- this function applies an algebraic system transformation
    applyST f (EQs (m,c)) = EQs (f (m,c))
    
    {-  ##################
       #JOIN SUB ROUTINE#
      ################## -}    
    linearCombinationVar4join:: Int -> [Double]
    -- VARIABLE ORDER := [V|W1|W2|lambda1|lambda2]
    -- single constraint coefficients for V = W1 + W2
    -- varN is |W1|=|W2|=|V|
    -- [V   |W1   | W2    |l1|l2]
    -- [-1..|11...|11.....|0| 0]
    linearCombinationVar4join varN = (replicate varN (-1))++ [0,0]++(ones (2*varN))

    lambdaRule4join :: Int -> [Double]
    -- VARIABLE ORDER := [V|W1|W2|lambda1|lambda2]
    -- single constraint coefficients for 1 = lambda1 + lambda2
    -- varN is |W1|=|W2|=|V|
    -- [V   |W1   | W2    |l1|l2]
    -- [00..|00...|00.....|1| 1]
    lambdaRule4join varN = (zeros (4*varN)) ++ [1,1]

    sysCoef4join :: EQs -> Int -> RowForm Double
    -- VARIABLE ORDER := [V|W1|W2|lambda1|lambda2]
    --it performs JUST the coefficients matrix of the join system
    --this function is a subroutine for explicit join
    -- position : Int variabel (in our case just 0-1) that provide the position of the poblem in
    --            variable vector. 
    --            i.e. : in the system { prob1 ^ prob2 ^ ...} the two problems have different set of variables
    --                   hence it's importa have an order in the variable vector [x_1var,x_2var], to remain 
    --                   consistent to the linearization [W1,W2] (see the notes at page 109)
    -- row i = [V    |W1   | W2    |l1|l2]
    -- row i = [00..|M1i   |0...   |-ci|0 ] or [00..|00..  |M2i   |0|-ci]
    sysCoef4join (EQs (m,c)) position = 
        foldr 
            (\(mh,ch) sr -> (
                let 
                    coefficients_W1W2 = (if position==0 
                        then mh++(zeros varN)
                        else (zeros varN)++mh)
                    coef_lambdas = if position==0 then [-ch,0] else [0,-ch]
                in 
                    ((zeros varN)++coefficients_W1W2++coef_lambdas):sr
                 )) [] zipMC            
        where 
            zipMC = zip m c
            varN = length (head m) -- cardinality of V
        
    
    explicit_join :: EQs -> EQs -> EQs -- lub
    --computes the joining system without performing any elimination (page 109)
    -- CONSTRAINt ORDER { V=W1+W2 ^ l1+l2=1 ^ sys1 ^ sys2} (for complexity reasons)
    
    EQs (m1,c1) `explicit_join` EQs (m2,c2) = 
        EQs (lin_comb_coef:lambdas_coef:sys1_coef++sys2_coef, constants_vector)
        where 
            sys1_coef = sysCoef4join (EQs (m1,c1)) 0
            sys2_coef = sysCoef4join (EQs (m2,c2)) 1
            varN = length (head m1)
            constraintsN1 = length m1
            constraintsN2 = length m2
            lin_comb_coef = linearCombinationVar4join varN
            lambdas_coef = lambdaRule4join varN
            constants_vector = 0:1:(zeros (constraintsN1+constraintsN2))
    {-  ######################
       #JOIN SUB ROUTINE END#
      ###################### -}  
    
    instance AbsDomainR EQs where 
        -- top :: EQs
        top = EQs ([],[])
        -- bottom :: EQs
        bottom = EQsBottom
        -- (<=) :: EQs -> EQs -> Bool
        -- A C= B <-> A meet B = A p.108
        EQs (m1,c1) <= EQs (m2,c2) = (EQs (m1,c1) `meet` EQs (m2,c2)) == EQs (m1,c1) 
        --NOTE: this comparision has sense iff both the side are in a rowEchelon form

        -- join :: EQs -> EQs -> EQs -- abs lub
        sys1 `join` sys2 = applyST rowEchelonForm (explicit_join sys1 sys2)
        -- meet :: EQs -> EQs -> EQs 
        EQs (m1,c1) `meet` EQs (m2,c2) = EQs (rowEchelonForm (m1++m2, c1++c2))

    