module Parsing where
import Exp

import Control.Applicative (some, (<|>), Alternative (many))
import Data.Char (isAlpha, isAlphaNum)
import Lab2
import Distribution.Simple.Utils (xargs)
import Data.Maybe(fromJust)
import Data.List (intercalate)

testParse :: Parser a -> String -> Maybe a
testParse p s
    = case apply p s of
        [] -> Nothing
        (a, _):_ -> Just a


var :: Parser Var
var = do
    x <- identifier (satisfy isAlpha) (satisfy isAlphaNum) <|> symbol "+" <|> symbol "-"
    return (Var x)
-- >>> testParse var "b is a var"
-- Var {getVar = "b"}

varExp :: Parser ComplexExp
-- echivalente
-- varExp = do  
--     v <- identifier (satisfy isAlpha) (satisfy isAlphaNum)
--     return (CX (Var v))

-- varExp = do  
--     v <- var
--     return (CX v)

varExp = do CX <$> var
-- >>> testParse varExp "b is a var"
-- CX (Var {getVar = "b"})

lambdaExp :: Parser ComplexExp
lambdaExp =  do
    char '/'
    v <- var
    lexeme (string "->")
    x <- lexeme expr
    return (CLam v x)
-- >>> testParse lambdaExp "/x -> x"
-- CLam (Var {getVar = "x"}) (CX (Var {getVar = "x"}))

letExp :: Parser ComplexExp
letExp = do
    lexeme (string "let")
    someWhiteSpace
    v <- lexeme var
    lexeme (string ":=")
    x1 <- lexeme expr
    someWhiteSpace
    lexeme (string "in")
    someWhiteSpace
    x2 <- lexeme expr
    return (Let v x1 x2)
-- >>> testParse letExp "let x := y in z"
-- Let (Var {getVar = "x"}) (CX (Var {getVar = "y"})) (CX (Var {getVar = "z"}))

letrecExp :: Parser ComplexExp
letrecExp = do
    lexeme (string "letrec")
    someWhiteSpace
    v <- lexeme var
    lexeme (string ":=")
    x1 <- lexeme expr
    someWhiteSpace
    lexeme (string "in")
    someWhiteSpace
    x2 <- lexeme expr
    return (LetRec v x1 x2)
-- >>> testParse letrecExp "letrec x := y in z"
-- LetRec (Var {getVar = "x"}) (CX (Var {getVar = "y"})) (CX (Var {getVar = "z"}))

listExp :: Parser ComplexExp
listExp = brackets list
    where 
        list = do
            xs <- commaSep expr
            return (List xs)
-- >>> testParse listExp "[a,b,c]"
-- List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})]

natExp :: Parser ComplexExp
natExp = do 
    n <- natural
    return (Nat (fromIntegral n))
-- >>> testParse natExp "223 a"
-- Nat 223

parenExp :: Parser ComplexExp
parenExp = parens expr
-- >>> testParse parenExp "(a)"
-- CX (Var {getVar = "a"})

basicExp :: Parser ComplexExp
basicExp = letExp <|> letrecExp <|> lambdaExp <|> varExp <|> natExp <|> listExp <|> parenExp
-- >>> testParse basicExp "[a,b,c]"
-- List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})]

expr :: Parser ComplexExp
expr = do
    x <- some basicExp
    return (foldl1 CApp x)

-- >>> testParse expr "/x -> [x,y,z]"
-- CLam (Var {getVar = "x"}) (List [CX (Var {getVar = "x"}),CX (Var {getVar = "y"}),CX (Var {getVar = "z"})])

exprParser :: Parser ComplexExp
exprParser = whiteSpace *> expr <* endOfInput
-- >>> testParse exprParser "let x := 28 in /y -> + x y"
-- Let (Var {getVar = "x"}) (Nat 28) (CLam (Var {getVar = "y"}) (CApp (CApp (CX (Var {getVar = "+"})) (CX (Var {getVar = "x"}))) (CX (Var {getVar = "y"}))))




