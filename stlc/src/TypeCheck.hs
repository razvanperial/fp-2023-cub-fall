module TypeCheck where

import Syntax
import qualified Data.Map as M
import Control.Monad (guard)
import Control.Applicative ((<|>))

type Env = M.Map String Type

typeCheckEmpty :: Term String -> Either String Type
typeCheckEmpty = typeCheck M.empty

typeCheck :: Env -> Term String -> Either String Type
typeCheck env (Var v) =
  case M.lookup v env of
    Just t -> Right t
    Nothing -> Left ("Variable '" ++ v ++ "' not found in the environment.")
typeCheck env (Abs x t b) = do
  let env' = M.insert x t env
  t1 <- typeCheck env' b
  return $ Arrow t t1
typeCheck env (App m n) = do
  t1 <- typeCheck env m
  t2 <- typeCheck env n
  case t1 of
    Arrow t11 t12 | t2 == t11 -> Right t12
    _ -> Left "Application type mismatch."
typeCheck _ (BoolLit _) =
  Right Bool
typeCheck env (If c t e) = do
  ct <- typeCheck env c
  if ct == Bool
    then do
      tt <- typeCheck env t
      et <- typeCheck env e
      if tt == et
        then return tt
        else Left "Branches of 'if' expression have different types."
    else Left "Condition in 'if' expression is not of type Bool."

typeCheck env (Let x t1 t2) = do
  t1' <- typeCheck env t1
  let env' = M.insert x t1' env
  typeCheck env' t2
