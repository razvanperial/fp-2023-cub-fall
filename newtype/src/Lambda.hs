module Lambda where 

import Data.List
import Text.Printf 

data Term 
  = Var String 
  | App Term Term 
  | Abs String Term
  deriving (Eq)

instance Show Term where 
  show (Var x) = x
  show (Abs x t) = printf "λ%s.%s" x (show t)
  show (App t1 t2) = printf "(%s) (%s)" (show t1) (show t2)

alphaEq :: Term -> Term -> Bool
alphaEq (Var x) (Var y) = x == y
alphaEq (App m1 n1) (App m2 n2) = alphaEq m1 m2 && alphaEq n1 n2
alphaEq (Abs x1 body1) (Abs x2 body2) =
  let freshVar = findFreshVar x1 (x2 : freeVars body1 ++ freeVars body2)
      renamedBody1 = renameVar x1 freshVar body1
      renamedBody2 = renameVar x2 freshVar body2
  in alphaEq renamedBody1 renamedBody2
alphaEq _ _ = False

freeVars :: Term -> [String]
freeVars (Var x) = [x]
freeVars (App m n) = freeVars m `union` freeVars n
freeVars (Abs x body) = freeVars body \\ [x]

renameVar :: String -> String -> Term -> Term
renameVar oldVar newVar (Var x) 
  | x == oldVar = Var newVar 
  | otherwise = Var x
renameVar oldVar newVar (App m n) = 
  App (renameVar oldVar newVar m) (renameVar oldVar newVar n)
renameVar oldVar newVar (Abs x body) 
  | x == oldVar = Abs newVar (renameVar oldVar newVar body) 
  | otherwise = Abs x (renameVar oldVar newVar body)

findFreshVar :: String -> [String] -> String
findFreshVar x usedVars
  | x `notElem` usedVars = x
  | otherwise = findFreshVar (x ++ "'") usedVars

newtype Alpha = Alpha { getTerm :: Term }

instance Eq Alpha where 
  Alpha x == Alpha y = alphaEq x y

run x y = do 
  let xstr = show x 
  let ystr = show y  
  putStrLn $ printf "%s %s %s" xstr (if x == y then "==" else "/=") ystr
  putStrLn $ printf "%s is %salpha-equivalent to %s\n" xstr (if Alpha x == Alpha y then "" else "not ") ystr
  
main :: IO ()
main = do
  let term1 = Abs "x" (App (Var "x") (Var "y"))
  let term2 = Abs "z" (App (Var "z") (Var "y"))
  let term3 = Abs "a" (App (Var "a") (Var "b"))
  run term1 term1
  run term1 term2 
  run term1 term3