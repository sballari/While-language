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
        deriving (Show, Eq)

    data BExpr = 
        True 
        | False 
        | Eq AExpr AExpr
        | LessEq AExpr AExpr
        | Neg BExpr
        | And BExpr BExpr
        deriving (Show, Eq)

    data Stm = 
        Assign Name AExpr
        | Skip
        | Comp Stm Stm
        | Cond BExpr Stm Stm -- if b then S1 else S2     
        | While BExpr Stm -- while b do S
        deriving (Show, Eq)