module WhileParser where 
    import Control.Applicative
    import Data.Char
    import WhileStructures as WS
    import qualified Data.Set 


    newtype Parser a = P (String -> [(a,String)])

    resultP :: Parser a -> String -> a
    resultP p i = (\[(x,y)]->x) (parse p i)

    parse :: Parser a -> String -> [(a,String)]
    parse (P parser) inp = parser inp

    item :: Parser Char
    item = P( \inp -> case inp of 
                    [] -> []
                    (x:xs) -> [(x,xs)]) 

    instance Functor Parser where 
        -- fmap :: (a->b) -> Parser a -> Parser b
        fmap f p = P (\inp -> case (parse p inp) of 
                            [] -> []
                            [(a,rest)] -> [(f a, rest)]  
                            ) 

    instance Applicative Parser where 
        --pure :: a -> Parser a 
        pure f = P (\inp -> [(f, inp)])
        -- <*> :: Parser (a->b) -> Parser a -> Parser b
        funp <*> pa = P (\inp -> case (parse funp inp) of
                                [] -> []
                                [(f, rest)] -> parse (fmap f pa) rest                         
            )

    instance Monad Parser where 
        -- >>= :: Parser a -> (a -> Parser b) -> Parser b
        pa >>= f = P (\inp -> case parse pa inp of 
                                [] -> []
                                [(a,rest)] -> parse (f a) rest )


    instance Alternative Parser where
        --empty :: Parser a 
        empty = P( \inp -> [])
        -- <|> :: Parser a -> Parser a -> Parser a
        p <|> p' = P ( \inp -> case parse p inp of 
                                [] -> parse p' inp 
                                [(a,rest)] -> [(a,rest)])
        
    sat :: (Char -> Bool) -> Parser Char
    sat p = item >>= \x -> if p x then return x else empty

    digit :: Parser Char 
    digit = sat isDigit 

    lower :: Parser Char 
    lower = sat isLower

    upper :: Parser Char 
    upper = sat isUpper

    letter :: Parser Char 
    letter = sat isAlpha

    alphanum :: Parser Char 
    alphanum = sat isAlphaNum

    char :: Char -> Parser Char
    char c = sat (==c)

    string :: String -> Parser String
    string [] = return []
    string (x:xs) = char x >>= \x' ->
                    string xs >>= \xs' ->
                        return (x:xs) 

    keywords :: [String]
    keywords = []

    isKeyWord :: String -> Bool 
    isKeyWord s = elem s keywords

    var :: Parser String
    var = do l <- letter
             ls <- (many (alphanum <|> (char '_')))
             if (isKeyWord (l:ls)) then empty
             else return (l:ls)

    nat :: Parser Int
    nat = some digit >>= \x ->
            return (read x)
    
    int :: Parser Int
    int = ((char '+' <|> char '-') >>= \sgn ->
            nat >>= \num ->
            return (if sgn=='-' then -num else num))
        <|> nat 
        
    space :: Parser ()
    space = many (sat isSpace) >>= \x->return ()

    token ::Parser a -> Parser a 
    token p = space >> -- ignoro il risultato
            p >>= \t ->
            space >>
            return t

    variable :: Parser String 
    variable = token var

    natural :: Parser Int
    natural = token nat

    integer :: Parser Int
    integer = token int

    symbol :: String -> Parser String
    symbol xs = token (string xs)

    --While specific Parser 
    --------------------------------------------------------------------------
    --AExpr

    parseAExpr :: Parser (AExpr)
    parseAExpr = parseAExpr1 
            
    parseAExpr1 :: Parser (AExpr)
    parseAExpr1 = 
        do 
            a <- parseAExpr2 --se fosse parse Aexpr1 loop
            op <- symbol "+" <|> symbol "-"
            b <- parseAExpr1
            return (if op == "+" then Sum a b else Sum a (Minus b))
        <|>
            parseAExpr2
            
    parseAExpr2 :: Parser (AExpr)
    parseAExpr2 = 
        pure(\a op b -> if op=="*" then Mul a b else Div a b) 
                <*> parseAExpr3
                <*> (symbol "*" <|> symbol "/")
                <*> parseAExpr2   
        <|>
        parseAExpr3
    
    parseAExpr3 :: Parser (AExpr) --espressioni aritmetiche atomiche
    parseAExpr3 = 
        fmap Num natural <|> 
        pure (\x y -> Minus y) <*> symbol "-" <*> parseAExpr <|>
        pure (\ a b c d e -> Range b d) <*> symbol "[" <*> integer <*> symbol "," <*> integer <*> symbol "]" <|>
        fmap Var variable <|>
        pure(\a b c->b) <*> symbol "(" <*> parseAExpr <*> symbol ")"

    --------------------------------------------------------------------------
    --BExpr
    -- il parser dei booleani ora e' il top del top
    parseBExpr :: Parser (BExpr)
    parseBExpr = parseBExpr1

    parseRelOp :: Parser (AExpr -> AExpr -> BExpr)
    parseRelOp = 
        fmap (\_->Eq) (symbol "=")          <|>
        fmap (\_->NotEq) (symbol "!=")      <|>
        fmap (\_->LessEq) (symbol "<=")     <|>
        fmap (\_->MoreEq) (symbol ">=")     <|>
        fmap (\_->More) (symbol ">")        <|>
        fmap (\_->Less) (symbol "<")        


    parseBExpr1 :: Parser (BExpr)
    parseBExpr1 = pure(\a op c-> case op of 
                                    "&" -> And a c
                                    "|" -> Or a c ) 
                                    <*> parseBExpr2
                                    <*> (symbol "&" <|> symbol "|") 
                                    <*> parseBExpr1
                  <|> parseBExpr2

    parseBExpr2 :: Parser (BExpr)
    parseBExpr2 = 
        do 
            a <- parseAExpr
            op <- parseRelOp
            b <- parseAExpr
            return (op a b)
        <|> parseBExpr3 

    parseBExpr3 :: Parser (BExpr)
    parseBExpr3 = 
        fmap (\x->WTrue) (symbol "true") <|>
        fmap (\x->WFalse) (symbol "false") <|>
        pure(\a b c->b) <*> symbol "(" <*> parseBExpr <*> symbol ")" <|>
        pure(\x y -> Neg y)<*> symbol "!" <*> parseBExpr3 
    
    --------------------------------------------------------------------------
    --Stm
    
    parseStms :: Parser (Stm)
    parseStms = 
        pure(\a b c -> Comp a c) <*> parseStm <*> symbol ";" <*> parseStms
        <|> parseStm

    parseStm :: Parser (Stm)
    parseStm = 
        parseAssignment <|>
        parseSkip <|>
        parseCond <|>
        parseWhile <|>
        parseAssert <|>
        pure(\a b c->b) <*> symbol "(" <*> parseStms <*> symbol ")"



    parseAssignment :: Parser (Stm)
    parseAssignment = pure(\a b c -> Assign a c) <*> variable <*> symbol ":=" <*> parseAExpr
    
    parseSkip :: Parser (Stm)
    parseSkip = fmap (\x->Skip) (symbol "skip")

    parseAssert :: Parser (Stm)
    parseAssert = do 
                    symbol "assert"
                    b <- parseBExpr
                    return (Assert b)

    parseCond :: Parser(Stm)
    parseCond = 
        do 
            symbol "if"
            b <- parseBExpr
            symbol "then"
            s1 <- parseStm
            symbol "else"
            s2 <- parseStm
            return (Cond b s1 s2)

    parseWhile :: Parser(Stm)
    parseWhile = 
        do
            symbol "while" 
            b <- parseBExpr
            symbol "do"
            body <- parseStm
            return (While b body)
    
    {- ##################
    #   UTILITA' STM    #
    ################## -}


    variables :: Stm -> [Name]
    --trova tutte le variabili che compaiono in Stm; serve per inizializzarle a top.
    variables stm = Data.Set.elems set
        where 
            set = Data.Set.fromList (variablesS stm)

    variablesS :: Stm -> [Name]
    variablesS (Assign x v) = [x] ++ (variablesA v)
    variablesS (Skip) = []
    variablesS (Comp s1 s2) = (variablesS s1) ++ (variablesS s2)
    variablesS (Cond b s1 s2) = (variablesB b) ++ (variablesS s1) ++ (variablesS s2)
    variablesS (While b s) = (variablesB b) ++ (variablesS s)
    variablesS (Assert b) = (variablesB b) 

    variablesA :: AExpr -> [Name]
    variablesA (Sum a1 a2) = (variablesA a1) ++ (variablesA a2)
    variablesA (Div a1 a2) = (variablesA a1) ++ (variablesA a2)
    variablesA (Mul a1 a2) = (variablesA a1) ++ (variablesA a2)
    variablesA (Minus a1)  = (variablesA a1)
    variablesA (Var x) = [x]
    variablesA (Num _) = []
    variablesA (Range _ _) = []

    variablesB :: BExpr -> [Name]
    variablesB WTrue = [] 
    variablesB WFalse = []
    variablesB (Eq a1 a2) = (variablesA a1) ++ (variablesA a2)
    variablesB (LessEq a1 a2) = (variablesA a1) ++ (variablesA a2)
    variablesB (Less a1 a2) = (variablesA a1) ++ (variablesA a2)
    variablesB (NotEq a1 a2) = (variablesA a1) ++ (variablesA a2)
    variablesB (More a1 a2) = (variablesA a1) ++ (variablesA a2)
    variablesB (MoreEq a1 a2) = (variablesA a1) ++ (variablesA a2)
    variablesB (Neg b1) = variablesB b1
    variablesB (And b1 b2) = (variablesB b1) ++ (variablesB b2)
    variablesB (Or b1 b2) = (variablesB b1) ++ (variablesB b2)
    