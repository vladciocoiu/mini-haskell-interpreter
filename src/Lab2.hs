module Lab2 where

import Data.Char
import Data.Tuple
import Control.Applicative

newtype Parser a = Parser { apply :: String -> [(a, String)] }


satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser go
    where
        go [] = [] 
        go (c:input)
            | p c = [(c, input)]
            | otherwise = []

-- accepts any character
anychar :: Parser Char
anychar = Parser go
    where
        go [] = []
        go (c:input) = [(c, input)] 

-- only accepts the character ch
char :: Char -> Parser Char
char ch = Parser go
    where 
        go [] = []
        go (c:input)
            | c == ch = [(c, input)]
            | otherwise = []

-- accepts any string
anystring :: Parser String
anystring = Parser go
    where 
        go [] = []
        go str = [(str, "")]

-- accepts any digit
digit :: Parser Char
digit = Parser go
    where
        go [] = []
        go (c:input)
            | isDigit c = [(c, input)]
            | otherwise = []

-- accepts space / tab / newline
space :: Parser Char
space = Parser go
    where
        go [] = []
        go (c:input)
            | isSpace c = [(c, input)]
            | otherwise = []

-- accepts + or -
plusMinus :: Parser Char
plusMinus = Parser go
    where
        go [] = []
        go (c:input)
            | c == '+' || c == '-' = [(c, input)]
            | otherwise = []

-- accepts the empty string
endOfInput :: Parser ()
endOfInput = Parser go
    where
        go "" = [((), "")]
        go _ = []

instance Functor Parser where
    fmap f pa = Parser (\input -> [(f a, rest) | (a, rest) <- apply pa input])

instance Applicative Parser where
    pure a = Parser (\input -> [(a, input)])
    pf <*> pa = Parser (\input -> [(f a, resta) | (f, restf) <- apply pf input, (a, resta) <- apply pa restf])

parse :: Parser a -> String -> Either String a
parse p str
    | null (apply (p <* endOfInput) str) = Left "String not completely parsed / ambiguous parsing"
    | otherwise = Right (fst (head (apply p str)))

instance Monad Parser where
    pa >>= k = Parser (\input -> [(b, restb) | (a, resta) <- apply pa input, (b, restb) <- apply (k a) resta])


digitBetweenParans :: Parser Int
digitBetweenParans = do
    char '('
    d <- digit
    char ')'
    return (digitToInt d)

signedDigit :: Parser Int
signedDigit = do
    sign <- plusMinus
    d <- digit
    return (if sign == '+' then digitToInt d else -(digitToInt d))

string :: String -> Parser String
string [] = return [] 
string (ch:s) = pure (:) <*> char ch <*> string s


instance Alternative Parser where
    empty = Parser (const [])
    p <|> p' = Parser (\input -> apply p input ++ apply p' input)

naiveNatural :: Parser Int
naiveNatural = string2Int <$> naiveNat
  where
    naiveNat = pure (:) <*> digit <*> naiveNat
            <|> pure (:[]) <*> digit
    string2Int = foldl (\n d -> 10 * n + digitToInt d) 0

-- removes some spaces or no spaces
whiteSpace :: Parser ()
whiteSpace = many space *> return ()

-- removes some spaces
someWhiteSpace :: Parser ()
someWhiteSpace = some space *> return ()

-- parses a natural number (one or more digits)
nat :: Parser Int
nat = string2Int <$> some digit
    where 
    string2Int = foldl (\n d -> 10 * n + digitToInt d) 0


-- applies a parser and removes whitespace after
lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace

-- parses a natural number and skips the space after it
natural :: Parser Int
natural = lexeme nat

-- Parses the string and skips whiteSpace after it
symbol :: String -> Parser String
symbol = lexeme . string

-- Parses the string, skips whiteSpace, returns unit
reserved :: String -> Parser ()
reserved s = symbol s *> return ()

-- parses comma and removes whitespace
comma :: Parser ()
comma = reserved ","

-- parses the argument between parantheses and removes whitespace
parens :: Parser a -> Parser a
parens p = reserved "(" *> p <* reserved ")"

-- same as parens but with square brackets
brackets :: Parser a -> Parser a
brackets p = reserved "[" *> p <* reserved "]"

-- one or more instances of p, separated with comma
commaSep1 :: Parser a -> Parser [a]
commaSep1 p
  = do
    a <- p
    as <- many (comma *> p)
    return (a : as)

-- zero or more instances of p, separated with comma
commaSep :: Parser a -> Parser [a]
commaSep p = commaSep1 p <|> pure []

-- parses an identifier with a given first character parser and other characters parser
ident :: Parser Char -> Parser Char -> Parser String
ident identStart identLetter
  = do
    s <- identStart
    ls <- many identLetter
    return (s:ls)

-- same as ident but removes whitespace
identifier :: Parser Char -> Parser Char -> Parser String
identifier start letter = lexeme (ident start letter)

semi :: Parser ()
semi = reserved ";"

-- one or more instances separated by ;
semiSep1 :: Parser a -> Parser [a]
semiSep1 p
    = do
        a <- p
        as <- many (semi *> p)
        return (a : as)

-- reads from file and applies parser
parseFromFile :: Parser a -> FilePath -> IO (Either String a)
parseFromFile parser file
    = do
    str <- readFile file
    case apply parser str of
        [] -> return $ Left "Cannot parse"
        (a,_):_ -> return $ Right a

haskellId :: Parser String
haskellId = identifier (satisfy isAlpha) (satisfy isAlphaNum)

haskellOp :: Parser String
haskellOp = identifier opSymbol opSymbol
  where
    opSymbol = satisfy isOp
    isOp = (`elem` "`~!@#$%^&*_+=|<>.?/")

