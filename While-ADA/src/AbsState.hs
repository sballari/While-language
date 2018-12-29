module AbsState where  
  import AbsDomain as AD
  import Data.List

  type VarName = String

  data AbsState a = S [(VarName, a)] | Top | Bottom deriving (Show, Eq)
  -- a should be the abstract domain
  -- haskell doesn't allow type constraint in data type


  
  -- instance Functor AbsState where
  --   -- fmap :: (a->b) -> AbsState a -> AbsState b
  --   fmap f as = case as of 
  --                 S xs -> S [(n,f v)|(n,v)<-xs] 
  --                 AbsState.Top -> AbsState.Top
  --                 AbsState.Bottom -> AbsState.Bottom
  -- instance Applicative AbsState where
  --   -- pure :: a -> AbsState a
  --   pure a = S ["", a]
  --   -- <*> :: AbsState (a->b) -> AbsState a -> AbsState b
  --   f <*> a = 


  alter :: AbsState a -> String -> a -> AbsState a
  alter AbsState.Bottom name v = AbsState.Bottom
  alter AbsState.Top name v = AbsState.Top
  alter (S[]) name v = S [(name, v)]
  alter (S(x:xs)) name v = if (fst x)==name then S ((name,v):xs) 
                            else 
                              case (alter (S xs) name v) of 
                                S xs -> S(x:xs)
                                AbsState.Top -> AbsState.Top
                                AbsState.Bottom -> AbsState.Bottom

                            
                          
  lookUp :: (UndefSup a) => AbsState a -> String -> a  
  lookUp AbsState.Bottom name = undef
  lookUp AbsState.Top name = undef 
  lookUp (S xs) name = case lookup name xs of
                      Nothing -> undef
                      Just x -> x 

  class UndefSup a where
    undef :: a 
  
  -- component wise extensions
  instance Eq a => AbsDomain (AbsState a) where 
    (<=) x y = True
    union x y = x
    intersection x y = x
    top = AbsState.Top
    bottom = AbsState.Bottom