{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-  ######################
       #condC SUB ROUTINE   #
      ###################### -}  
module PolyUtils where
    import WhileStructures

    data LMonomial =  MC Double | M Double String deriving(Eq, Show)
    type LPolynomial = [LMonomial]
    
    coefM :: LMonomial -> Double
    coefM (MC c) = c
    coefM (M c _) = c
    varM :: LMonomial -> String
    varM (M _ x) = x

    coefVar :: String -> LPolynomial -> Maybe Double 
    --the function will take the first coefficient of var v found
    coefVar v l = foldr (\m rc -> case m of (M c v') -> if v==v' then Just c else rc; (MC c) -> rc) Nothing l 
    -----------------------------------------------------------
    multiply :: LPolynomial -> LPolynomial -> Maybe LPolynomial
    --[x1,x2,x3]*[y1,y2,y3]
    multiply l1 [] = Just l1 
    multiply [] l2 = Just l2
    multiply l1 l2 = 
        foldr (\h1 rs -> (++) <$> (mpMul h1 l2) <*> rs) (Just []) l1 

    
    mpMul :: LMonomial -> LPolynomial -> Maybe LPolynomial
    -- c*[y1,y2,y3]
    mpMul (MC c) lp= Just (
        foldr (\m ms-> 
            case m of
                MC c' -> (MC (c*c')) : ms
                M c' varName'-> (M (c*c') varName') : ms
        ) [] lp  )
    
    mpMul (M c varName) lp = 
        foldr (\m ms->
            case m of
                MC c' ->  (:) <$> pure (M (c*c') varName) <*> ms
                {- 
                    TODO : i think it's possible remove the pure 
                    in a more comprehensible way (non sadistic version) :  
                    let Just l = ms in Just ((M (c*c') varName): l)
                -}
                M c' varName' -> Nothing
        ) (Just []) lp
    
    -----------------------------------------------------------------
    divide :: LPolynomial -> LPolynomial -> Maybe LPolynomial
    -- this code isn't understandable like my girlfriend
    divide l1 [] = Just l1
    divide [] l2 = Just l2
    divide l1 l2 
        -- l1,l2 not empty
        | len1 == 0 && len2 == 0 =  Just [MC (coefM (head l1c) / coefM (head l2c))]
        | len2 == 0 = Just [ case m of M c v -> M (c/coefM (head l2c)) v ; MC c -> MC (c/coefM (head l2c)) |m <- l1c]
        | len1 == 1 && len2 ==1 && (head vars1) == (head vars2) = 
            case (head l1c) of 
                (M c v) -> 
                    let Just v_in_l2c = coefVar v l2c in 
                    Just [M (c/v_in_l2c) v, MC (coefM (l1c!!1) / fst (sumCoefX0 l2c))]
                (MC c) -> 
                    let Just cv = coefVar (varM (l1c!!1)) l2c in
                    Just [MC (c/fst (sumCoefX0 l2c)), M (coefM (l1c!!1) / cv ) (varM (l1c!!1))]
        | otherwise = Nothing -- TODO : I'm not sure is correct
        where
            l1c = compact l1 -- fundamental!
            l2c = compact l2 -- fundamental!
            vars1 = varList (compact l1c)
            vars2 = varList (compact l2c)
            len1 = length vars1
            len2 = length vars2

    varList :: LPolynomial -> [String]
    varList = foldr (\m rc ->
        case m of 
            (MC c) -> rc
            (M c name) -> name:rc
        ) []
-----------------------------------------------------------------------------
    compact :: LPolynomial -> LPolynomial
    {-
        3x+2x+3+4 --> 5x+7
    -}
    compact [] = []
    compact ((M c var):ms) = ( M var_coef var) : (compact pol_no_var) 
        where 
            rc = sumCoef var ms
            var_coef = (fst rc) + c
            pol_no_var = snd rc
    compact ((MC c):ms) = (MC coefX0): (compact pol_no_X0)
        where
            rc = sumCoefX0 ms
            coefX0 = c + (fst rc)
            pol_no_X0 = snd rc


    sumCoef :: String -> LPolynomial -> (Double, LPolynomial)
    {-
    it sums the coef of variables name and 
    return also the polynomial without var name
    -}
    sumCoef name [] = (0,[])
    sumCoef name (m:ms) = 
        let 
           rc = sumCoef name ms
           rSum = fst rc
           rPol = snd rc
        in
        case m of
            (M c var) -> 
                if var==name then (c + rSum , rPol)
                else (rSum,(M c var):rPol)
            m -> (rSum, m:rPol)
    sumCoefX0 :: LPolynomial -> (Double,LPolynomial)
    sumCoefX0 [] = (0,[]) 
    sumCoefX0 (m:ms) = 
        let 
           rc = sumCoefX0 ms
           rSum = fst rc
           rPol = snd rc
        in
        case m of
            (MC c) -> (c + rSum , rPol) 
            m -> (rSum, m:rPol)
-----------------------------------------------------------------------------
    minimize :: AExpr -> Maybe LPolynomial
    {-
        descr:
        this function tries to transform the given aexpr in a linear expression.
        returns: 
        If the aexpr contains a non deterministic variable the fun returs Nothing
        If the aexpr isn't linear the fun returns Nothing
        Otherwise the function returns a compact rappresentation of aexpr
    -}
    minimize (Num x) = Just [MC (fromIntegral x)] --TODO: just to run the code
    minimize (Var name) = Just [M 1 name]
    minimize (Sum a1 a2) = 
        do 
            mp1 <- (minimize a1) 
            mp2 <- (minimize a2)
            return (compact (mp1++mp2))
    minimize (Mul a1 a2) = 
        do 
            mp1 <- (minimize a1) 
            mp2 <- (minimize a2)
            fmap compact (multiply mp1 mp2)
    minimize (Div a1 a2) = 
        do
            mp1 <- (minimize a1) -- l pol.
            mp2 <- (minimize a2) -- l pol.
            mp1 `divide` mp2
    minimize (Minus expr) = 
        do
            mp <- minimize expr 
            return (foldr (\m rc -> case m of (MC c) -> (MC (-c)):rc; (M c v) -> (M (-c) v):rc ) [] (compact mp))
    minimize (Range a b) = Nothing -- TODO : check if this is the correct approach. I'm not pretty sure.
---------------------------------------------------------------------
    order :: [String] -> LPolynomial -> ([Double],Double)
    --IMPORTANT : the order func wrongs when there are vars in the new constrain that doesn't appear in the ordering list
    -- order the l pol according to the variables list given as first parameter. 
    -- the output tuple is the list of coefficients and  the  the constant term 
    order ord lp = 
        if compatibility ord lp then (coefList, const)
        else error "[order] you are loosing some coefficient of the starting pol. "
            where 
                lpc = compact lp
                coefList = foldr (\v rc -> case (coefVar v lpc) of Just c -> c:rc; Nothing -> 0:rc ) [] ord 
                const = (fst.sumCoefX0) lpc

    compatibility :: [String] -> LPolynomial -> Bool
    -- check if every var in lPol is present in the given list 
    -- Vars[lPol] C= ordList 
    compatibility ord lPol = 
        foldr (\m rc -> 
            case m of 
                (M c v) -> (elem v ord) && rc
                (MC c) -> rc
        ) True lPol 
