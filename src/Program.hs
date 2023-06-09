
module Program where
import Exp
import Lab2 ( Parser, endOfInput, whiteSpace, reserved, semiSep1, string, lexeme, semiSep1, semi, haskellId )
import Parsing ( expr, var, testParse, exprParser )
import Sugar ( desugarExp, desugarVar )
import Eval ( substitute, normalize )

import Control.Applicative ( Alternative(..) )
import System.IO ( stderr, hPutStrLn )
import qualified Data.Map.Strict as Map

data Definition = Definition
  { defHead :: Var
  , defArgs :: [Var]
  , defBody :: ComplexExp
  }
  deriving (Show)

definition :: Parser Definition
definition = do
  h <- haskellId
  args <- many haskellId
  reserved ":="
  body <- expr
  return (Definition (Var h) (map Var args) body)

-- >>> testParse definition "id := \\x -> x"
-- Just (Definition {defHead = Var {getVar = "id"}, defArgs = [], defBody = CLam (Var {getVar = "x"}) (CX (Var {getVar = "x"}))})

-- >>> testParse definition "id x := x"
-- Just (Definition {defHead = Var {getVar = "id"}, defArgs = [Var {getVar = "x"}], defBody = CX (Var {getVar = "x"})})

-- >>> testParse definition "const x y := x"
-- Just (Definition {defHead = Var {getVar = "const"}, defArgs = [Var {getVar = "x"},Var {getVar = "y"}], defBody = CX (Var {getVar = "x"})})

program :: Parser [Definition]
program = whiteSpace *> semiSep1 definition <* semi <* endOfInput

-- >>> testParse program "    id x := x ; const x y := x"
-- Nothing

-- >>> testParse program "    id x := x ; const x y := x ;"
-- Just [Definition {defHead = Var {getVar = "id"}, defArgs = [Var {getVar = "x"}], defBody = CX (Var {getVar = "x"})},Definition {defHead = Var {getVar = "const"}, defArgs = [Var {getVar = "x"},Var {getVar = "y"}], defBody = CX (Var {getVar = "x"})}]

definitionExp :: Definition -> ComplexExp
definitionExp def
  | null (defArgs def) = defBody def
  | otherwise = CLam (head (defArgs def)) (definitionExp (Definition (defHead def) (tail (defArgs def)) (defBody def)))

-- >>> definitionExp (Definition {defHead = Var {getVar = "const"}, defArgs = [Var {getVar = "x"},Var {getVar = "y"}], defBody = CX (Var {getVar = "x"})})
-- CLam (Var {getVar = "x"}) (CLam (Var {getVar = "y"}) (CX (Var {getVar = "x"})))

type Environment = Map.Map IndexedVar Exp

programEnv :: [Definition] -> Environment
programEnv pgm = Map.fromList [(desugarVar (defHead def), desugarExp (definitionExp def)) | def <- pgm]

normalizeEnv :: Environment -> Exp -> Exp
normalizeEnv env (X v) = case Map.lookup v env of
  Nothing -> X v
  Just value -> normalizeEnv env value
normalizeEnv env (Lam v x) = Lam v (normalizeEnv env x)
normalizeEnv env (App (Lam v x) y) = normalizeEnv env (substitute v y x)

-- in order to implement the call-by-name strategy
-- we have to normalize x first, then the whole App, and only then y
normalizeEnv env (App x y)
    | normalizeEnv env x == x = App x (normalizeEnv env y)
    | otherwise = normalizeEnv env (App (normalizeEnv env x) y)