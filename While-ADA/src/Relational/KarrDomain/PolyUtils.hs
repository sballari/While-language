{-  ######################
       #condC SUB ROUTINE   #
      ###################### -}  
module PolyUtils where
    data LMonomial =  MC Double | M Double String deriving(Eq, Show)
    type LPolynomial = [LMonomial]

    multiply :: LPolynomial -> LPolynomial -> Maybe LPolynomial

    multiply l1 [] = Just l1
    multiply [] l2 = Just l2
    multiply l1 l2 = 
        foldr (\h rs -> 
            case h of 
                MC c -> Just ( (mol1 c) ++ rs )
                M c varName -> 
                    case (mol2 c varName) of 
                        Nothing -> Nothing 
                        Just p -> Just (p ++ rs)
            ) (Just []) l1 
        where
            --[x1,x2,x3]*[y1,y2,y3]
            mol1 = 
                \c-> (foldr (\m ms-> 
                case m of
                    MC c' -> (MC (c*c') : ms)
                    M c' varName'-> (M (c*c') varName' : ms)
                ) [] l2)  -- c1*[y1,y2,y3]
            mol2 = 
                \c varName -> (foldr (\m ms->
                case m of
                    MC c' ->  Just ((M (c*c') varName): ms)
                    M c' varName' -> Nothing
                ) (Just []) l2)

    {-
    minimize :: AExpr -> Maybe LPolynomial
    minimize (Num x) = Just [MC x]
    minimize (Var name) = Just [M 1 name]
    minimize (Mul a1 a2) = 
        if m1==Nothing | m2==Nothing then Nothing
        else 
            --
        where 
            m1 = (minimize a1) 
            m2 = (minimize a2)
    -- minimize (Minus ex) = 
    -}