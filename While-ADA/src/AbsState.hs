module AbsState where  
  import AbsDomain as AD
  import qualified  WhileStructures as WS
  import Data.List

  type VarName = String

  data AbsState a = S [(VarName, a)] | Bottom deriving (Show, Eq)
  -- a should be the abstract domain
  -- haskell doesn't allow type constraint in data type


  alter :: AbsState a -> String -> a -> AbsState a
  alter AbsState.Bottom name v = AbsState.Bottom
  alter _ _ bottom = AbsState.Bottom 
  alter (S[]) name v = S [(name, v)]
  alter (S(x:xs)) name v = if (fst x)==name then S ((name,v):xs) 
                            else 
                              case (alter (S xs) name v) of 
                                S xs -> S(x:xs)
                                AbsState.Bottom -> AbsState.Bottom

                            
                          
  lookUp :: (AbsDomain a) => AbsState a -> String -> a  
  lookUp AbsState.Bottom name = bottom
  lookUp (S xs) name = case (lookup name xs) of
                          Nothing -> top
                          Just x -> x 

  
  -- component wise extensions
  (<=) :: (AbsDomain a) => AbsState a -> AbsState a -> Bool  
  (<=) AbsState.Bottom _  = True
  (<=) (S xs) (S ys) = (xs == ys)  
  (<=) _ _ = False

  union :: (AbsDomain a) => AbsState a -> AbsState a -> AbsState a
  union x AbsState.Bottom = x
  union AbsState.Bottom y = y  
  union (S xs) (S ys) =S [(a, (AD.union b d)) | (a,b)<-xs , (c,d)<-ys, a==c]

  intersection :: (AbsDomain a) => AbsState a -> AbsState a -> AbsState a
  intersection _ AbsState.Bottom = AbsState.Bottom
  intersection AbsState.Bottom _ = AbsState.Bottom
  intersection (S xs) (S ys) = let i = [(a, (AD.intersection b d)) | (a,b)<-xs , (c,d)<-ys, a==c] in 
                                if (findEl bottom i) then AbsState.Bottom
                                else (S i) 

  -- abstract semantics of arithmetic expression
  absAS :: (AbsDomain a) => WS.AExpr -> (AbsState a)-> a
  absAS (WS.Sum ex1 ex2) s = absSum (absAS ex1 s) (absAS ex2 s)
  absAS (WS.Mul ex1 ex2) s = absMul (absAS ex1 s) (absAS ex2 s)
  absAS (WS.Minus ex) s = absMinus (absAS ex s)
  absAS (WS.Div ex1 ex2) s = absDiv (absAS ex1 s) (absAS ex2 s)
  absAS (WS.Num x) s = omega (WS.Num x)
  absAS (WS.Range x y) s = omega (WS.Range x y)
  absAS (WS.Var n) s = lookUp s n

  -- abstract semantics of boolean expressions
  -- states filter
  absBS :: (AbsDomain a) => WS.BExpr -> (AbsState a)-> (AbsState a)
  absBS WS.True s = s
  absBS WS.False s = AbsState.Bottom
  absBS (WS.Eq e1 e2) s | (a1 /= top || a1 /= bottom) &&
                          (a2 /= top || a2 /= bottom) &&
                          (a1 /= a2) = AbsState.Bottom
                        | (a1 == bottom || a2 == bottom) = AbsState.Bottom 
                        | otherwise = s
        where a1 = (absAS e1 s)
              a2 = (absAS e2 s)

  absBS (WS.LessEq e1 e2) s | (a1 /= top && a2 /= top) && 
                              (a1 AD.> a2) = AbsState.Bottom 
                            | a1 == bottom && a2 == bottom = AbsState.Bottom 
                            | otherwise = s
    where a1 = (absAS e1 s)
          a2 = (absAS e2 s)
  
  absBS (WS.And b1 b2) s = absBS b2 (absBS b1 s) 
  absBS (WS.Neg b1) s = if (absBS b1 s) == s then AbsState.Bottom else s
  
  --utility function
  findEl :: (AbsDomain a) => a -> [(b,a)] -> Bool 
  findEl el xs = foldr (\x r-> (((snd x) == el) || r) ) False xs


    
