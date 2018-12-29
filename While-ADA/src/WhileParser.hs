module WhileParser where 
    import Control.Applicative
    import Data.Char
    import WhileStructures as WS


    newtype Parser a = P (String -> [(a,String)])

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
    token p = space >>= \s ->
            p >>= \t ->
            space >>= \s' ->
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
            a <- parseAExpr2
            op <- symbol "+" <|> symbol "-"
            b <- parseAExpr1
            if op == "+" then return (Sum a b)
            else return (Min a b)
        <|>
            parseAExpr2
            
    parseAExpr2 :: Parser (AExpr)
    parseAExpr2 = 
        pure(\a op b -> Mul a b) <*> parseAExpr3 <*> symbol "*" <*> parseAExpr2   
        <|>
        parseAExpr3
    
    parseAExpr3 :: Parser (AExpr)
    parseAExpr3 = 
        fmap Num integer <|> 
        pure (\ a b c d e -> Range b d) <*> symbol "[" <*> natural <*> symbol "," <*> natural <*> symbol "]" <|>
        fmap Var variable <|>
        pure(\a b c->b) <*> symbol "(" <*> parseAExpr <*> symbol ")"

    --------------------------------------------------------------------------
    --BExpr
                
    parseBExpr :: Parser (BExpr)
    parseBExpr = 
        parseBExpr1 <|>
        parseBExpr2 <|>
        parseBExpr3


    parseBExpr1 :: Parser (BExpr)
    parseBExpr1 = 
        do 
            a <- parseAExpr
            op <- symbol "=" <|> symbol "<="
            b <- parseAExpr
            if op == "=" then return (Eq a b)
            else return (LessEq a b)
    
    parseBExpr2 :: Parser (BExpr)
    parseBExpr2 = pure(\a b c-> And a c) <*> parseBExpr3 <*> symbol "&" <*> parseBExpr2
                  <|> parseBExpr3
    
    parseBExpr3 :: Parser (BExpr)
    parseBExpr3 = 
        fmap (\x->WS.True) (symbol "true") <|>
        fmap (\x->WS.False) (symbol "false") <|>
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
        pure(\a b c->b) <*> symbol "(" <*> parseStms <*> symbol ")"



    parseAssignment :: Parser (Stm)
    parseAssignment = pure(\a b c -> Assign a c) <*> variable <*> symbol ":=" <*> parseAExpr
    
    parseSkip :: Parser (Stm)
    parseSkip = fmap (\x->Skip) (symbol "skip")

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
    