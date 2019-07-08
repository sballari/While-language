module AbsState where  
  import AbsDomain as AD
  import qualified  WhileStructures as WS
  import Data.List

  
  type VarName = String

  
  data AbsState a = S [(VarName, a)] | Bottom -- smashed Bottom
  -- a should be the abstract domain
  -- haskell doesn't allow type constraint in data type

  instance Show a => Show (AbsState a) where
    show Bottom = "\8869"++"state"
    show (S xs) = "{" ++ (foldr (\(vn,val) rs -> -- serve veramente ??
                            if rs == [] then vn++" \8712 "++show(val)
                            else vn++" \8712 "++show(val)++", "++rs) "" xs) ++ "}"

  varName = fst
  varValue = snd

  alter :: AbsDomain a => AbsState a -> String -> a -> AbsState a
  alter Bottom _ _ = Bottom
  alter (S[]) name v = S [(name, v)] -- non dovrebbe mai succedere
  alter (S(x:xs)) name v =  if (varName x)==name then S ((name,v):xs) 
                            else 
                              case (alter (S xs) name v) of 
                                S xs -> S(x:xs)
                                --AbsState.Bottom -> AbsState.Bottom                   
                          
  lookUp :: AbsDomain a => AbsState a -> String -> a
  lookUp AbsState.Bottom name = AD.bottom 
  -- x:=0/0;
  -- y:= x;
  -- if x = y then <-il filtro chiama abseval(x) abseval(y) che chiamamo lookUp sul bottomState
  --      //altrmenti bisogna modificare il filtro considerando il caso - molte cose hanno questo caso (per sicurezza)
  --     x := 1
  -- else x:= -2 
  lookUp (S []) name = AD.top
  lookUp (S (x:xs)) name = if (varName x) == name then (varValue x) else lookUp (S xs) name

   
  -- component wise extensions

  (<=) :: (AbsDomain a) => AbsState a -> AbsState a -> Bool  
  (<=) AbsState.Bottom _  = True
  (<=) (S xs) (S ys) = foldr (\(var,x) sr -> (x AD.<= (lookUp (S ys) var)) && sr ) True xs
  (<=) _ _ = False
         
  
  join :: (AbsDomain a) => AbsState a -> AbsState a -> AbsState a
  join x Bottom = x
  join Bottom y = y  
  join (S xs) (S ys) = S [(a, (AD.join b d)) | (a,b)<-xs , (c,d)<-ys, a==c]

  meet :: (AbsDomain a) => AbsState a -> AbsState a -> AbsState a
  meet _ AbsState.Bottom = AbsState.Bottom
  meet AbsState.Bottom _ = AbsState.Bottom
  meet (S xs) (S ys) =
    let i = [(a, (AD.meet b d)) | (a,b)<-xs , (c,d)<-ys, a==c] in 
                                if (findEl bottom i) then AbsState.Bottom
                                else (S i) 
                                  
  widening :: (AbsDomain a) => AbsState a -> AbsState a -> AbsState a
  widening Bottom y = y
  widening x Bottom = x
  widening (S xs) (S ys) =
    S [(a, (AD.widening b d)) | (a,b)<-xs , (c,d)<-ys, a==c]
                          
  --utility function
  findEl :: (Eq a) => a -> [(b,a)] -> Bool 
  findEl el = foldr (\x r-> (((snd x) == el) || r) ) False
  {-
  -- class UndefSup a where
  --   undef :: a 
  -}

  instance  AbsDomain a => Eq (AbsState a) where
    -- == :: a -> a -> Bool
    S xs == S ys = (foldr (\x sr -> (elem x ys) && sr ) True xs ) && (foldr (\y sr-> (elem y xs) && sr ) True ys)
    Bottom == Bottom = True
    _ == _ = False