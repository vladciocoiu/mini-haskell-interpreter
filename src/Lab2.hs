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

-- accepta orice caracter
anychar :: Parser Char
anychar = Parser go
    where
        go [] = []
        go (c:input) = [(c, input)] 

--- | acceptă doar caracterul dat ca argument
char :: Char -> Parser Char
char ch = Parser go
    where 
        go [] = []
        go (c:input)
            | c == ch = [(c, input)]
            | otherwise = []

anystring :: Parser String
anystring = Parser go
    where 
        go [] = []
        go str = [(str, "")]

--- | acceptă o cifră
digit :: Parser Char
digit = Parser go
    where
        go [] = []
        go (c:input)
            | isDigit c = [(c, input)]
            | otherwise = []

--- | acceptă un spațiu (sau tab, sau sfârșit de linie)
space :: Parser Char
space = Parser go
    where
        go [] = []
        go (c:input)
            | isSpace c = [(c, input)]
            | otherwise = []

--- | acceptă + sau -
plusMinus :: Parser Char
plusMinus = Parser go
    where
        go [] = []
        go (c:input)
            | c == '+' || c == '-' = [(c, input)]
            | otherwise = []

--- | succes doar dacă am șirul de intrare este vid
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
    | null (apply (p <* endOfInput) str) = Left "Sirul de intrare nu a fost complet consumat sau parsare ambigua"
    | otherwise = Right (fst (head (apply p str)))
-- parse p str
--     | [(a, "")] <- apply (p <* endOfInput) str = Right a
--     | otherwise = Left "Sirul de intrare nu a fost complet consumat sau parsare ambigua"

instance Monad Parser where
    pa >>= k = Parser (\input -> [(b, restb) | (a, resta) <- apply pa input, (b, restb) <- apply (k a) resta])


cifraIntreParanteze :: Parser Int
cifraIntreParanteze = do
    char '('
    d <- digit
    char ')'
    return (digitToInt d)

cifraSemn :: Parser Int
cifraSemn = do
    sign <- plusMinus
    d <- digit
    return (if sign == '+' then digitToInt d else -(digitToInt d))

string :: String -> Parser String
-- string "" = Parser go
--     where
--         go c = [("", c)]

-- string (s:str) = Parser go
--     where
--         go [] = []
--         go (c:input)
--             | null (apply (string str) input) = []
--             | c == s = [(c : fst (head (apply (string str) input)), snd (head (apply (string str) input)))]
--             | otherwise = []
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

-- | Elimină zero sau mai multe apariții ale lui `space`
whiteSpace :: Parser ()
whiteSpace = many space *> return ()

-- elimina unul sau mai multe spatii
someWhiteSpace :: Parser ()
someWhiteSpace = some space *> return ()

-- | parses a natural number (one or more digits)
nat :: Parser Int
nat = string2Int <$> some digit
    where 
    string2Int = foldl (\n d -> 10 * n + digitToInt d) 0


-- | aplică un parser, și elimină spațiile de după
lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace

-- | parses a natural number and skips the space after it
natural :: Parser Int
natural = lexeme nat

-- | Parses the string and skips whiteSpace after it
symbol :: String -> Parser String
symbol = lexeme . string

-- | Parses the string, skips whiteSpace, returns unit
reserved :: String -> Parser ()
reserved s = symbol s *> return ()

-- | parsează virgulă, eliminând spațiile de după
comma :: Parser ()
comma = reserved ","

-- | parsează argumentul intre paranteze rotunde
--   elimină spațiile de după paranteze
parens :: Parser a -> Parser a
parens p = reserved "(" *> p <* reserved ")"

-- | parsează argumentul intre paranteze pătrate
--   elimină spațiile de după paranteze
brackets :: Parser a -> Parser a
brackets p = reserved "[" *> p <* reserved "]"

-- | una sau mai multe instanțe, separate de virgulă,
--   cu eliminarea spațiilor de după fiecare virgulă
--   intoarce lista obiectelor parsate
commaSep1 :: Parser a -> Parser [a]
commaSep1 p
  = do
    a <- p
    as <- many (comma *> p)
    return (a : as)

-- | zero sau mai multe instanțe, separate de virgulă,
--   cu eliminarea spațiilor de după fiecare virgulă
--   intoarce lista obiectelor parsate
commaSep :: Parser a -> Parser [a]
commaSep p = commaSep1 p <|> pure []

-- | date fiind parsere pentru prima literă si pentru felul literelor următoare
--   scrieți un parser pentru un identificator
ident :: Parser Char -> Parser Char -> Parser String
ident identStart identLetter
  = do
    s <- identStart
    ls <- many identLetter
    return (s:ls)

-- | ca mai sus, dar elimină spatiile de după
identifier :: Parser Char -> Parser Char -> Parser String
identifier start letter = lexeme (ident start letter)

semi :: Parser ()
semi = reserved ";"

-- | una sau mai multe instanțe, separate de punct-și-virgulă,
-- cu eliminarea spațiilor de după fiecare punct-și-virgulă
-- intoarce lista obiectelor parsate
semiSep1 :: Parser a -> Parser [a]
semiSep1 p
    = do
        a <- p
        as <- many (semi *> p)
        return (a : as)

-- | citește un fișier și aplică analiza sintactică specificată asupra
-- conținutului său
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

