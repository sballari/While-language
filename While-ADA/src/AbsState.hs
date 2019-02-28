module AbsState where  
  import AbsDomain as AD
  import qualified  WhileStructures as WS
  import Data.List

  
  type VarName = String

  
  data AbsState a = S [(VarName, a)] | Bottom deriving (Show, Eq) -- a non può essere Bottom
  -- a should be the abstract domain
  -- haskell doesn't allow type constraint in data type


  alter :: AbsDomain a => AbsState a -> String -> a -> AbsState a
  alter AbsState.Bottom _ _ = AbsState.Bottom
  alter (S[]) name v = S [(name, v)]
  alter (S(x:xs)) name v =  if (fst x)==name then S ((name,v):xs) 
                            else 
                              case (alter (S xs) name v) of 
                                S xs -> S(x:xs)
                                AbsState.Bottom -> AbsState.Bottom

   -- D# = (V->(B#\{AD.Bottom})) U AS.Bottom                         
                          
  lookUp :: AbsDomain a => AbsState a -> String -> a
  -- lookUp AbsState.Bottom name = AD.bottom -- bottom?
  lookUp (S []) name = AD.top
  lookUp (S (x:xs)) name = if (fst x) == name then (snd x) else lookUp (S xs) name

   
  -- component wise extensions

  (<=) :: (AbsDomain a) => AbsState a -> AbsState a -> Bool  
  (<=) AbsState.Bottom _  = True
  (<=) (S xs) (S ys) = foldr (\(var,x) sr -> (x Prelude.<= (lookUp (S ys) var)) && sr ) True xs
  (<=) _ _ = False
         
  -- (<=) (S []) (S []) = True
  -- (<=) (S (x:xs)) (S (y:ys)) = if lookUp (S x) x <= lookUp (S y) y then (<=) (S (xs)) (S (ys)) else False 
  -- per ogni V ho che lookUp (S xs) V <= lookUp (S ys) V
  -- S ed S' possono avere cardinalità diverse oppure l'ordine delle tuple diverso?
  
  
 
  join :: (AbsDomain a) => AbsState a -> AbsState a -> AbsState a
  join x Bottom = x
  join Bottom y = y  
  join (S xs) (S ys) =S [(a, (AD.join b d)) | (a,b)<-xs , (c,d)<-ys, a==c]

  meet :: (AbsDomain a) => AbsState a -> AbsState a -> AbsState a
  meet _ AbsState.Bottom = AbsState.Bottom
  meet AbsState.Bottom _ = AbsState.Bottom
  meet (S xs) (S ys) = -- TODO da scrivere in maniera + efficiente
    let i = [(a, (AD.meet b d)) | (a,b)<-xs , (c,d)<-ys, a==c] in 
                                if (findEl bottom i) then AbsState.Bottom
                                else (S i) 
                                  
  widening :: (AbsDomain a) => AbsState a -> AbsState a -> AbsState a
  widening Bottom y = y
  widening x Bottom = x
  widening (S xs) (S ys) =
    S [(a, (AD.widening b d)) | (a,b)<-xs , (c,d)<-ys, a==c]
{-
  -- abstract semantics
  absAS :: (AbsDomain a{-, UndefSup a-}) => WS.AExpr -> (AbsState a)-> a
  absAS (WS.Sum ex1 ex2) s = absSum (absAS ex1 s) (absAS ex2 s)
  absAS (WS.Mul ex1 ex2) s = absMul (absAS ex1 s) (absAS ex2 s)
  absAS (WS.Minus ex) s = absMinus (absAS ex s)
  absAS (WS.Div ex1 ex2) s = absDiv (absAS ex1 s) (absAS ex2 s)
  absAS (WS.Num x) s = omega (WS.Num x)
  absAS (WS.Range x y) s = omega (WS.Range x y)
  absAS (WS.Var n) s = lookUp s n
  -}                           
  --utility function
  findEl :: (Eq a) => a -> [(b,a)] -> Bool 
  findEl el = foldr (\x r-> (((snd x) == el) || r) ) False
  {-
  -- class UndefSup a where
  --   undef :: a 
  -}