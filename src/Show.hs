module Show where

import Exp
import Data.List (intercalate)

inParens :: String -> String
inParens x = "(" ++ x ++ ")"

showVar :: Var -> String
showVar (Var x) = x

showExp :: ComplexExp -> String
showExp (Nat n) = show n
showExp (CX v) = showVar v
showExp (CLam v x) = inParens ("/" ++ showVar v ++ " -> " ++ showExp x)
showExp (CApp a b) = inParens (showExp a ++ " " ++ showExp b)
showExp (Let v x y) = inParens ("l " ++ showVar v ++ " = " ++ showExp x ++ " in " ++ showExp y)
showExp (LetRec v x y) = inParens ("lr " ++ showVar v ++ " = " ++ showExp x ++ " in " ++ showExp y)
showExp (List l) = "[" ++ intercalate "," (map showExp l) ++ "]"