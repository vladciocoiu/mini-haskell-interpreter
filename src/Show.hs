module Show where

import Exp
import Data.List (intercalate)

showVar :: Var -> String
showVar (Var x) = x

showExp :: ComplexExp -> String
showExp (Nat n) = show n
showExp (CX v) = showVar v
showExp (CLam v x) = "/" ++ showVar v ++ " -> " ++ showExp x
showExp (CApp a b) = showExp a ++ "(" ++ showExp b ++ ")"
showExp (Let v x y) = "l " ++ showVar v ++ " = " ++ showExp x ++ " in " ++ showExp y
showExp (LetRec v x y) = "lr " ++ showVar v ++ " = " ++ showExp x ++ " in " ++ showExp y
showExp (List x) = "[" ++ intercalate "," (map showExp x) ++ "]"