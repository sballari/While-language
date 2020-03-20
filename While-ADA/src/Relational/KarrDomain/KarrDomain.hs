--{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module KarrDomain where
    import AbsDomainR
    import MatrixUtilities
    import WhileStructures
    import PolyUtils
    import Text.Printf
    
    data EQs = EQs (RowForm Double,[Double],[String]) -- <M,X> in a row-echelon form,
    -- [String]: variables name in the same order they appears in X
                 | EQsBottom deriving (Eq)

    {-  ##################
       #Assign sub routines     #
      ################## -} 

    varPos :: EQs -> String -> Maybe Int
    -- returns the position (from zero) of the given var as second arg in the system provided as first arg
    varPos eqs v = varPos' eqs v 0 
    varPos' :: EQs -> String -> Int -> Maybe Int
    -- returns the position of the var given as second arg in the system provide as first arg
    -- third arg is a counter, initialize with 0
    varPos' EQsBottom _ _ = Nothing
    varPos' (EQs (_,_,[])) var _ = Nothing
    varPos' (EQs (s,c,(v:vs))) var cnt =
        if (v==var) then (Just cnt)
            else varPos' (EQs (s,c,(vs))) var (cnt+1)

    assignUnbounded :: 
        EQs ->
        String -> -- var name
        EQs
    {-
        PRE :  EQs is in a row-echelon form
        descr: this function is semS[Vj <- [-inf,+inf]] 
        two case : 1 vj in leading pos, 2 vj not in leading pos
    -}
    assignUnbounded EQsBottom _ = EQsBottom
    assignUnbounded (EQs (rs,b,vars)) vj = 
        if is_leading then
            let (newRs,newB) = remove_Lc (rs,b) var_index in 
                (EQs (newRs,newB,vars))
        else {- is not in leading position -}
            let 
                augMatrix = transpose (b:(transpose rs)) -- aggiungo in testa per comodita'
                newAugM = log_elimination augMatrix (var_index+1) -- +1 perche ho messo b in testa
                -- de aumento la matrice
                newAugMT = (transpose newAugM)
                newRs = transpose (tail newAugMT)
                newB = head newAugMT
            in 
                (EQs (newRs,newB,vars))

        where 
            Just var_index = varPos (EQs (rs,b,vars)) vj
            vj_columnT = (transpose rs)!!var_index
            is_leading = if lead_check == (1,True) then True else  False
            lead_check  =  foldr (\c (sum,j01) ->
                        if j01 then 
                            case c of 
                                1 -> (sum+1,True)
                                0 -> (sum, True)
                                _ -> (0,False)
                        else (0,False)
                     ) (0,True) vj_columnT

    remove_Lc ([],[]) index_vj = ([],[])
    remove_Lc ((r:rs),(b:bs)) index_vj = 
        {-
        PRE : vj is in leading
        descr : remove the row i where a[i,indexVj] = 1
        -}
        if (r!!index_vj) == 0 then 
            let (rs',bs') = remove_Lc (rs,bs) index_vj in
            ((r:rs'),(b:bs'))
        else {- ==1 -} (rs,bs)
        
    inversion_coefficients ::   -- (invertible case)
        ([Double],Double) ->    -- assign expr e,  vj<-e
        Int ->                  -- index of vj
        Double ->               -- a_vj : coef of vj in e
        ([Double],Double)       -- inversion coefficients
    inversion_coefficients (coefficients,b) var_index a_vj = ((
                                    foldr (
                                        \(a,i) rc -> 
                                            if i==var_index then (1/a_vj):rc
                                            else (-a/a_vj):rc
                                        ) [] (zip coefficients [0..])
                                ), -b/a_vj)
    invertible_assign :: 
        Int -> -- index of the reexpressed variable
        ([Double],Double) -> -- re expressed coeff Vj_old = f(Vj_new), coef of f
        EQs -> --current state
        EQs -- updated state
    invertible_assign index re_expr (EQs (rs,bs,o)) =  
        let (rs',bs') = invertible_assign' index re_expr (rs,bs) in 
            EQs (rs',bs',o)
    invertible_assign' :: 
        Int -> ([Double],Double) -> (RowForm Double,[Double]) -> (RowForm Double,[Double])
    invertible_assign' index re_expr ([],[]) = ([],[])
    invertible_assign' index (inv_coef,inv_const_part)  ((r:rs),(b:bs)) = 
        let (rs',bs') = invertible_assign' index (inv_coef,inv_const_part) (rs,bs) in
            ((r':rs'),(b':bs'))
        where
            sys_vj_coef = r!!index --vj's coef in the system matrix
            additional_qts = foldr (\c rc-> (c*sys_vj_coef):rc) [] inv_coef
            additional_const_part = sys_vj_coef*inv_const_part
            r' = foldr (\(x1,x2,i) rc -> 
                                if i /= index then (x1+x2):rc 
                                else {- i == var_index -} x2:rc
                        ) [] (zip3 r additional_qts [0..])
            b' = b-additional_const_part
    non_invertible_assign :: -- vj <- e
        String  -> -- name of the var vj
        AExpr   -> -- aexpr e
        EQs     -> -- state before assignment
        EQs        -- result 
    non_invertible_assign var_name expr sys = 
            condC new_constr (assignUnbounded sys var_name) 
        where 
            new_constr = assign_to_newConstr var_name expr 

    assign_to_newConstr :: -- x<-e
        String  -> -- name of var x
        AExpr   -> -- expression e
        BExpr      -- x-e=0
    {- utility function for non invertible case -}
    assign_to_newConstr var_name expr =
        Eq (Sum (Var var_name) (Minus expr)) (Num 0) -- vj - e = 0

     
    {-  ##################
       #print stuff     #
      ################## -} 

    applyST :: ((RowForm Double,[Double],[String])-> (RowForm Double,[Double],[String])) -> EQs -> EQs
    -- this function applies an algebraic system transformation
    applyST f (EQs (m,c,o)) = EQs (f (m,c,o))
    {-  ##################
       #print stuff     #
      ################## -} 
    printCell :: Double -> String
    printCell el = if el>=0 then (' ':strN) else strN
        where strN = (printf "%.2f" el)

    print_row :: [Double] -> String
    --TODO: strange behavior 
    print_row = foldr (\el r-> (printCell el)++"\t"++r ) "" 

    instance Show EQs where
        --show ::EQs -> String
        show EQsBottom = "\8869"++"Karr"
        show (EQs (m,c,o)) = 
            "\n"++(show o)++ 
            "\n"++(foldr (\(r,ch) rest -> (print_row r)++"|"++(show ch)++"\n"++rest  ) "" z)
            where
                z = zip m c 
    {-  ##################
       #JOIN SUB ROUTINE#
      ################## -}    
    linearCombinationVar4join:: Int -> [Double]
    -- VARIABLE ORDER := [V|W1|W2|lambda1|lambda2]
    -- single constraint coefficients for V = W1 + W2
    -- varN is |W1|=|W2|=|V|
    -- [V   |W1   | W2    |l1|l2]
    -- [-1..|11...|11.....|0| 0]
    linearCombinationVar4join varN = (replicate varN (-1))++(ones (2*varN))++ [0,0]

    lambdaRule4join :: Int -> [Double]
    -- VARIABLE ORDER := [V|W1|W2|lambda1|lambda2]
    -- single constraint coefficients for 1 = lambda1 + lambda2
    -- varN is |W1|=|W2|=|V|
    -- [V   |W1   | W2    |l1|l2]
    -- [00..|00...|00.....|1| 1]
    lambdaRule4join varN = (zeros (3*varN)) ++ [1,1]

    sysCoef4join :: EQs -> Int -> RowForm Double
    -- VARIABLE ORDER := [V|W1|W2|lambda1|lambda2]
    -- it performs JUST the coefficients matrix of the join system
    -- this function is a subroutine for explicit join
    -- position : Int variable (in our case just 0-1) that provide the position of the problem in
    --            variable vector. 
    --            i.e. : in the system { prob1 ^ prob2 ^ ...} the two problems have different set of variables
    --                   hence it's import have an order in the variable vector [x_1var,x_2var], to remain 
    --                   consistent to the linearization [W1,W2] (see the notes at page 109)
    -- row i = [V    |W1   | W2    |l1|l2]
    -- row i = [00..|M1i   |0...   |-ci|0 ] or [00..|00..  |M2i   |0|-ci]
    sysCoef4join (EQs (m,c,_)) position = 
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
    -- computes the joining system without performing any elimination (page 109)
    -- CONSTRAINt ORDER { V=W1+W2 ^ l1+l2=1 ^ sys1 ^ sys2} (for complexity reasons)
    -- the vars list refers only to V (the first n vars in x) and NOT to the others aux vars like l1,l2,w1,w2
    -- TODO : is it correct that
    
    EQs (m1,c1,o) `explicit_join` EQs (m2,c2,o')  
        -- the two system must have the same variables ordering 
        | o /= o' = error "not compatible systems"
        | otherwise = EQs (lin_comb_coef:lambdas_coef:sys1_coef++sys2_coef, constants_vector,o)
        where 
            sys1_coef = sysCoef4join (EQs (m1,c1,o)) 0
            sys2_coef = sysCoef4join (EQs (m2,c2,o)) 1
            varN = length (head m1)
            constraintsN1 = length m1
            constraintsN2 = length m2
            lin_comb_coef = linearCombinationVar4join varN
            lambdas_coef = lambdaRule4join varN
            constants_vector = 0:1:(zeros (constraintsN1+constraintsN2))

    {-  #######################
       #AbsDomainR instance  #
      ####################### -}  
    
    instance AbsDomainR EQs where 
        -- top :: EQs
        top = EQs ([],[],[])
        -- bottom :: EQs
        bottom = EQsBottom
        -- (<=) :: EQs -> EQs -> Bool
        -- A C= B <-> A meet B = A p.108
        EQs (m1,c1,o) <= EQs (m2,c2,o') 
            | o /=o' = error "not compatible systems"
            | otherwise = (EQs (m1,c1,o) `meet` EQs (m2,c2,o)) == EQs (m1,c1,o) 
        --NOTE: this comparison has sense iff both the side are in a rowEchelon form

        -- join :: EQs -> EQs -> EQs -- abs lub
        sys1 `join` sys2 = applyST rowEchelonForm (explicit_join sys1 sys2)
        -- meet :: EQs -> EQs -> EQs 
        EQs (m1,c1,o) `meet` EQs (m2,c2,o') 
            | o /=o' = error "not compatible systems"
            | otherwise =EQs (rowEchelonForm (m1++m2, c1++c2,o))

        -- condC :: BExpr -> EQs  -> EQs
        {- 
            we handle only the affine case : (Sum{j in Vars} c*V[j] ) = b
            IMPORTANT : the condC wrongs when there are vars in the new constrain that doesn't appear in the starting system
            BExpr := ...| Eq AExpr AExpr |...
            TODO: we can't deal the non deterministic variable (i'm not sure about that)
            AExpr := ...|Sum AExpr AExpr | Var Name |Mul AExpr AExpr|Num Int|...
            TODO:here we have a Num Int because the analyzer was initially design just for Integer variable analyses
            we should change the parser in order to consider the real variable (and maybe adjust the NR analyzer) 
        -}
        condC (Eq a1 a2) (EQs (m,x,o)) = 
            case mc of 
                Nothing -> EQs (m,x,o) --the constraint is non-linear
                Just lp -> 
                    let (cl,const) = order o lp in 
                    EQs (m++[cl],x++[-const],o)
            where 
                mc = minimize (Sum a1 (Minus a2))        
        condC _ sys = sys --identity

        --assignS :: Stm -> a -> a 
        {-
            descr: Vj <- e
            we have different possibilities:
            1) e isn't a lin exrp -> identity function
            2) e is a linear expr ->
                a) invertible , z'=z+1 -> z=z'-1 -> substitution in EQs 
                b) not invertible, condC(Vj-e=0).assignUnb 
        -}
        assignS _ EQsBottom = EQsBottom
        assignS (Assign var_name e) (EQs (rs,bs,o)) = 
            case conversion of 
                Nothing -> (EQs (rs,bs,o))    -- case 1 
                Just l_pol ->   -- case 2
                    let
                        (coefficients,b) = order o l_pol
                        Just var_index = varPos (EQs (rs,bs,o)) var_name
                        a_vj = coefficients!!var_index
                    in 
                        if a_vj == 0 then 
                            -- case 2b
                            non_invertible_assign var_name e (EQs (rs,bs,o)) 
                        else 
                            -- case 2a
                            let 
                                --p111 
                                inv_c = inversion_coefficients (coefficients,b) var_index a_vj
                            in 
                                invertible_assign var_index inv_c (EQs (rs,bs,o))
                                


            where
                conversion = minimize e
                

        
        