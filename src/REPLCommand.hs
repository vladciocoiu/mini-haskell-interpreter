module REPLCommand where

import Text.Parsec.Language (emptyDef, LanguageDef)
import qualified Text.Parsec.Token as Token
import Text.Parsec (anyChar)
import Control.Applicative (many, (<|>))
import Lab2

data REPLCommand
    = Quit
    | Load String
    | Eval String
quitCommand :: Parser REPLCommand
quitCommand = 
  do
    symbol ":q"
    endOfInput
    return Quit

loadCommand :: Parser REPLCommand
loadCommand = 
  do
    symbol ":l"
    space
    s <- anystring
    endOfInput
    return (Load s)

evalCommand :: Parser REPLCommand
evalCommand = 
  do
    s <- identifier (satisfy (/= ':')) anychar
    endOfInput
    return (Eval s)
  

replCommand :: Parser REPLCommand
replCommand = do quitCommand <|> loadCommand <|> evalCommand
