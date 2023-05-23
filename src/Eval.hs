
module Eval where

import Exp
import Data.List ( union, delete, maximum, (\\) )

vars :: Exp -> [IndexedVar]
vars (X v) = [v]
vars (Lam v x) = v : vars x
vars (App x y) = vars x ++ vars y

-- >>> vars (Lam (makeIndexedVar "x") (X (makeIndexedVar "y")))
-- [IndexedVar {ivName = "x", ivCount = 0},IndexedVar {ivName = "y", ivCount = 0}]

-- >>> vars (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0})))
-- [IndexedVar {ivName = "x", ivCount = 0},IndexedVar {ivName = "y", ivCount = 0},IndexedVar {ivName = "z", ivCount = 0}]

-- >>> vars (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0})))
-- [IndexedVar {ivName = "x", ivCount = 0}]

freeVars :: Exp -> [IndexedVar]
freeVars = freeVars' []

freeVars' :: [IndexedVar] -> Exp -> [IndexedVar]
freeVars' boundVars (X v)
  | v `elem` boundVars = []
  | otherwise = [v] 
freeVars' boundVars (Lam v x) = freeVars' (v : boundVars) x
freeVars' boundVars (App x y) = freeVars' boundVars x ++ freeVars' boundVars y


-- >>> freeVars (Lam (makeIndexedVar "x") (X (makeIndexedVar "y")))
-- [IndexedVar {ivName = "y", ivCount = 0}]

-- >>> freeVars  (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0})))
-- [IndexedVar {ivName = "y", ivCount = 0},IndexedVar {ivName = "z", ivCount = 0}]

-- >>> freeVars (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0})))
-- [IndexedVar {ivName = "x", ivCount = 0}]

-- >>> freeVars (Lam (IndexedVar {ivName = "x", ivCount = 0}) (App (X (IndexedVar {ivName = "x", ivCount = 0})) (X (IndexedVar {ivName = "x", ivCount = 0}))))
-- []

occursFree :: IndexedVar -> Exp -> Bool
occursFree v x = v `elem` freeVars x

-- >>> makeIndexedVar "x" `occursFree` Lam (makeIndexedVar "x") (X (makeIndexedVar "y"))
-- False

-- >>> makeIndexedVar "y" `occursFree` Lam (makeIndexedVar "x") (X (makeIndexedVar "y"))
-- True
firstFreeNumber :: String -> [IndexedVar] -> Int
firstFreeNumber s list = 
        let
            nums = map ivCount (filter (\x -> ivName x == s) list)
            join = [0..maximum nums + 1] \\ nums
        in 
            if null nums || null join then 0
            else head join


freshVar :: IndexedVar -> [IndexedVar] -> IndexedVar
freshVar v list = IndexedVar (ivName v) (firstFreeNumber (ivName v) list)

-- >>> freshVar (makeIndexedVar "x") [makeIndexedVar "x"]
-- IndexedVar {ivName = "x", ivCount = 1}

-- >>> freshVar (makeIndexedVar "x") [makeIndexedVar "x", IndexedVar {ivName = "x", ivCount = 1}, IndexedVar {ivName = "y", ivCount = 2}] 
-- IndexedVar {ivName = "x", ivCount = 2}

renameVar :: IndexedVar -> IndexedVar -> Exp -> Exp
renameVar toReplace replacement (X v) = if v == toReplace then X replacement else X v
renameVar toReplace replacement (Lam v x) = Lam (if v == toReplace then replacement else v) (renameVar toReplace replacement x)
renameVar toReplace replacement (App x y) = App (renameVar toReplace replacement x) (renameVar toReplace replacement y)


-- >>> renameVar (IndexedVar {ivName = "x", ivCount = 0}) (IndexedVar {ivName = "z", ivCount = 0}) (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0})))
-- App (Lam (IndexedVar {ivName = "z", ivCount = 0}) (X (IndexedVar {ivName = "z", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0}))

substitute :: IndexedVar -> Exp -> Exp -> Exp
substitute toReplace replacement (X v) = if v == toReplace then replacement else X v
substitute toReplace replacement (Lam v x)
    | v == toReplace = Lam v x
    | v /= toReplace && v `notElem` freeVars replacement = Lam v (substitute toReplace replacement x)
    | otherwise = 
        let 
            fresh = freshVar v (vars replacement)
        in Lam fresh (substitute toReplace replacement (renameVar v fresh x))
substitute toReplace replacement (App x y) = App (substitute toReplace replacement x) (substitute toReplace replacement y)

-- substitute (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "z", ivCount = 0})) (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0})))
-- App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0}))

-- >>> substitute (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0})) (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0})))
-- App (Lam (IndexedVar {ivName = "x", ivCount = 1}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0}))
-- (/x -> y) z [y := x]  ----- (/x1 -> x) z
normalize :: Exp -> Exp
normalize (App (Lam v x) y) = substitute v y x
-- normalize (App (Lam v x) y) = substitute v (normalize y) (normalize x) -- for applicative order
normalize (App x y) = App (normalize x) (normalize y)
normalize (Lam v x) = Lam v (normalize x)
normalize (X v) = X v

-- >>> normalize (X (makeIndexedVar "x"))
-- X (IndexedVar {ivName = "x", ivCount = 0})

-- >>> normalize (App (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (App (Lam (IndexedVar {ivName = "y", ivCount = 0}) (App (X (IndexedVar {ivName = "y", ivCount = 0})) (X (IndexedVar {ivName = "y", ivCount = 0})))) (Lam (IndexedVar {ivName = "y", ivCount = 0}) (App (X (IndexedVar {ivName = "y", ivCount = 0})) (X (IndexedVar {ivName = "y", ivCount = 0}))))))
-- X (IndexedVar {ivName = "x", ivCount = 0})

