module WhileStructures where 

    type Name = String

    data AExpr = 
        Sum AExpr AExpr  
        | Mul AExpr AExpr 
        | Div AExpr AExpr
        | Minus AExpr 
        | Var Name 
        | Num Int -- integer numeric values (Z)
        | Range Int Int -- nondeterministic range
        deriving (Eq)

    data BExpr = 
        WTrue 
        | WFalse 
        | Eq AExpr AExpr
        | LessEq AExpr AExpr
        | Less AExpr AExpr
        | NotEq AExpr AExpr
        | More AExpr AExpr
        | MoreEq AExpr AExpr
        | Neg BExpr
        | And BExpr BExpr
        | Or BExpr BExpr
        deriving (Eq)

    data Stm = 
        Assign Name AExpr
        | Skip
        | Comp Stm Stm
        | Cond BExpr Stm Stm -- if b then S1 else S2     
        | While BExpr Stm -- while b do S
        | Assert BExpr
        deriving (Show, Eq)

    instance Show AExpr where
        show  (Sum a1 (Minus a2)) = (show a1)++" - "++show(a2)
        show  (Sum a1 a2) = show(a1)++" + "++show(a2)
        show  (Mul a1 a2) = show(a1)++" * "++show(a2)
        show  (Div a1 a2) = show(a1)++" / "++show(a2)
        show  (Minus a1) =  " - "++show(a1)
        show  (Var x) =  x
        show  (Num x) =  show x
        show  (Range a b) =  "["++(show a)++","++(show b)++"]"

    instance Show BExpr where
        show WTrue = "True"
        show WFalse = "False" 
        show (Eq a1 a2)= (show a1) ++ " = "  ++ (show a2)
        show (LessEq a1 a2)= (show a1) ++ " <= "  ++ (show a2)
        show (Less a1 a2)  = (show a1) ++ " < "  ++ (show a2)
        show (NotEq a1 a2) = (show a1) ++ " != "  ++ (show a2)
        show (More a1 a2)  = (show a1) ++ " > "  ++ (show a2)
        show (MoreEq a1 a2)= (show a1) ++ " >= "  ++ (show a2)
        show (Neg b)   = "! ("++(show b)++")"
        show (And b1 b2)   =  (show b1) ++ " AND "  ++ (show b2)
        show (Or b1 b2)= (show b1) ++ " OR "  ++ (show b2)




