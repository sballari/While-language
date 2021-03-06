module AbsState where  
  import AbsDomain as AD
  import qualified  WhileStructures as WS
  import Data.List

  
  type VarName = String

  
  data AbsState a = S [(VarName, a)] | Bottom deriving (Read)-- smashed Bottom
  -- a should be the abstract domain
  -- haskell doesn't allow type constraint in data type

  instance Show a => Show (AbsState a) where
    show Bottom = "\8869"++"state"
    show (S xs) = "{" ++ (foldr (\(vn,val) rs -> -- serve veramente ??
                            if rs == [] then vn++" \8712 "++show(val)
                            else vn++" \8712 "++show(val)++", "++rs) "" xs) ++ "}"

  varName = fst
  varValue = snd

  topVarsInit :: AbsDomain a => [VarName] -> AbsState a
  --stato con le variabile passate a top
  topVarsInit vars = foldr (\x st -> alter st x top) (S []) vars

  alter :: AbsDomain a => AbsState a -> String -> a -> AbsState a
  alter Bottom _ _ = Bottom
  alter (S[]) name v = S [(name, v)] 
  alter (S(x:xs)) name v =  if (varName x)==name then S ((name,v):xs) 
                            else 
                              case (alter (S xs) name v) of 
                                S xs -> S(x:xs)                  
                          
  lookUp :: AbsDomain a => AbsState a -> String -> a
  lookUp AbsState.Bottom name = AD.bottom 
  -- x:=0/0;
  -- y:= x;
  -- if x = y then 
  --      //il filtro chiama abseval(x) abseval(y) che chiamamo lookUp sul bottomState
  --      //altrmenti bisognerebbe aggiungere questa clausola ad ogni occorrenza di CondC 
  --     x := 1
  -- else x:= -2 
  lookUp (S []) name = AD.top
  lookUp (S (x:xs)) name = if (varName x) == name then (varValue x) else lookUp (S xs) name

   
  -- component wise extensions

  (<=) :: (AbsDomain a) => AbsState a -> AbsState a -> Bool  
  (<=) AbsState.Bottom _  = True
  (<=) (S xs) (S ys) = 
    foldr (\var sr-> ((lookUp (S xs) var)  AD.<= (lookUp (S ys) var) ) && sr ) True vars
    where 
      vars = (explicitVars (S xs)) `Data.List.union` (explicitVars (S ys))
  (<=) _ _ = False
         
  componentwise2StateOp :: AbsDomain a => (a -> a -> a) -> AbsState a -> AbsState a -> AbsState a
  componentwise2StateOp op (S xs) (S ys) = 
    S(do 
        var <- (explicitVars (S xs)) `Data.List.union` (explicitVars (S ys))
        let 
          a = lookUp (S xs) var 
          b = lookUp (S ys) var in
          return (var, op a b))

  join :: (AbsDomain a) => AbsState a -> AbsState a -> AbsState a
  join x Bottom = x
  join Bottom y = y  
  join (S xs) (S ys) = componentwise2StateOp AD.join (S xs) (S ys)

  meet :: (AbsDomain a) => AbsState a -> AbsState a -> AbsState a
  meet _ AbsState.Bottom = AbsState.Bottom
  meet AbsState.Bottom _ = AbsState.Bottom
  meet (S xs) (S ys) = 
    let (S i) = componentwise2StateOp AD.meet (S xs) (S ys) in 
    if (findEl bottom i) then AbsState.Bottom --controllo smashedBottom
                                else (S i)  
                                  
  widening :: (AbsDomain a) => AbsState a -> AbsState a -> AbsState a
  widening Bottom y = y
  widening x Bottom = x
  widening (S xs) (S ys) = componentwise2StateOp AD.widening (S xs) (S ys)
                          
  --utility function
  findEl :: (Eq a) => a -> [(b,a)] -> Bool 
  findEl el = foldr (\x r-> (((snd x) == el) || r) ) False

  explicitVars :: AbsState a -> [VarName]
  -- lista dei nomi presenti nello stato astratto
  explicitVars Bottom = []
  explicitVars (S []) = []
  explicitVars (S ((x,a):xs)) = x:(explicitVars (S xs))

  instance  AbsDomain a => Eq (AbsState a) where
    -- == :: a -> a -> Bool
    S xs == S ys = (foldr (\x sr -> (elem x ys) && sr ) True xs ) && (foldr (\y sr-> (elem y xs) && sr ) True ys)
    Bottom == Bottom = True
    _ == _ = False