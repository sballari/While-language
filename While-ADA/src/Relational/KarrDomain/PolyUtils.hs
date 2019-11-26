{-  ######################
       #condC SUB ROUTINE   #
      ###################### -}  
module PolyUtils where
    import WhileStructures

    data LMonomial =  MC Double | M Double String deriving(Eq, Show)
    type LPolynomial = [LMonomial]
    
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
    
    --TODO: Division
    -- divide :: LPolynomial -> LPolynomial -> Maybe LPolynomial
    -- divide l1 l2 = 

    compact :: LPolynomial -> LPolynomial
    {-
        3x+2x+3+4 --> 5x+7
    -}
    compact (M c var):ms = ( M (sumCoef var ms) var) : 
    compact (MC c):ms = 
    sumCoef :: VarName -> LPolynomial -> (Double, LPolynomial)
    {-
    it sums the coef of variables name and 
    return also the polynomial without vars name
    -}
    sumCoef name [] = (0,[])
    sumCoef name (m:ms) = 
        let 
           rc = sumCoef name ms
           rSum = fst rc
           rPol = snd rc
        in
        case m of
            (M c var) -> (c + rSum , rPol) 
            m -> (rSum, m:rPol)
    sumCoefX0 :: LPolynomial -> Double
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

    minimize :: AExpr -> Maybe LPolynomial
    minimize (Num x) = Just [MC (fromIntegral x)] --TODO: just to run the code
    minimize (Var name) = Just [M 1 name]
    minimize (Mul a1 a2) = 
        do 
            mp1 <- (minimize a1) 
            mp2 <- (minimize a2)
            multiply mp1 mp2
    -- minimize (Div a1 a2) = 
    --     do
    --         mp1 <- (minimize a1) 
    --         mp2 <- (minimize a2)

    