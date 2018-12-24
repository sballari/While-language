module AbsState where  
    import AbsDomain
    import Data.List

    type VarName = String

    type AbsState a =  [(VarName, a)]
    -- a should be the abstract domain
    -- haskell doesn't allow type constraint in data type

    alter :: AbsState a -> String -> a -> AbsState a
    alter [] name v = [(name, v)]
    alter (x:xs) name v = if (fst x) == name then (name,v):xs 
                          else x:(alter xs name v)
     
    lookUp :: (UndefSup a) => AbsState a -> String -> a  
    lookUp s name = case lookup name s of
                        Nothing -> undef
                        Just x -> x 

    class UndefSup a where
      undef :: a 
    
    