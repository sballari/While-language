module AbsState where  
  import AbsDomain as AD
  import Data.List

  type VarName = String

  data AbsState a = S [(VarName, a)] | Bottom deriving (Show, Eq)
  -- a should be the abstract domain
  -- haskell doesn't allow type constraint in data type


  alter :: AbsState a -> String -> a -> AbsState a
  alter AbsState.Bottom name v = AbsState.Bottom
  alter (S[]) name v = S [(name, v)]
  alter (S(x:xs)) name v = if (fst x)==name then S ((name,v):xs) 
                            else 
                              case (alter (S xs) name v) of 
                                S xs -> S(x:xs)
                                AbsState.Bottom -> AbsState.Bottom

                            
                          
  lookUp :: UndefSup a => AbsState a -> String -> a  
  lookUp AbsState.Bottom name = undef
  lookUp (S xs) name = case (lookup name xs) of
                          Nothing -> undef
                          Just x -> x 

  
  -- component wise extensions
  (<=) :: (AbsDomain a) => AbsState a -> AbsState a -> Bool  
  (<=) AbsState.Bottom _  = True
  (<=) (S xs) (S ys) = (xs == ys)  
  (<=) _ _ = False

  union :: (AbsDomain a) => AbsState a -> AbsState a -> AbsState a
  union x AbsState.Bottom = x
  union AbsState.Bottom y = y  
  union (S xs) (S ys) = S (Data.List.union xs ys)

  intersection :: (AbsDomain a) => AbsState a -> AbsState a -> AbsState a
  intersection _ AbsState.Bottom = AbsState.Bottom
  intersection AbsState.Bottom _ = AbsState.Bottom
  intersection (S xs) (S ys) = let i = intersect xs ys in 
                                if (findEl bottom i) then AbsState.Bottom
                                else (S i) 
                             
  --utility function
  findEl :: (AbsDomain a) => a -> [(b,a)] -> Bool 
  findEl el xs = foldr (\x r-> (((snd x) == el) || r) ) False xs
    
  class UndefSup a where
    undef :: a 