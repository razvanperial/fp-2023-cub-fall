module TypeCheck where

import Syntax
import qualified Data.Map as M
import Control.Monad (guard, unless)
import Control.Applicative ((<|>))
import Data.Either (isRight)
import Control.Applicative (Alternative(..))

type Env = M.Map String Type

-- implement error types to be returned by typeCheck
data TypeCheckError
  = UnknownError
  | VariableNotFound
  | ApplicationTypeMismatch
  | BranchesOfIfHaveDifferentTypes
  | ConditionNotBool
  deriving (Show, Eq)


typeCheckEmpty :: Term String -> Either TypeCheckError Type
typeCheckEmpty = typeCheck M.empty

typeCheck :: Env -> Term String -> Either TypeCheckError Type
typeCheck env (Var v) =
  case M.lookup v env of
    Just t -> Right t
    -- return error if variable is not found
    Nothing -> Left VariableNotFound
typeCheck env (Abs x t b) = do
  let env' = M.insert x t env
  t1 <- typeCheck env' b
  return $ Arrow t t1
typeCheck env (App m n) = do
  t1 <- typeCheck env m
  t2 <- typeCheck env n
  case t1 of
    Arrow t11 t12 | t2 == t11 -> Right t12
    _ -> Left ApplicationTypeMismatch
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
        else Left BranchesOfIfHaveDifferentTypes
    else Left ConditionNotBool

typeCheck env (Let x t1 t2) = do
  t1' <- typeCheck env t1
  let env' = M.insert x t1' env
  typeCheck env' t2

typeCheck env (Add m n) = do
  t1 <- typeCheck env m
  t2 <- typeCheck env n
  unless (t1 == Int && t2 == Int) (Left ApplicationTypeMismatch)
  return Int
typeCheck env (Sub m n) = do
  t1 <- typeCheck env m
  t2 <- typeCheck env n
  unless (t1 == Int && t2 == Int) (Left ApplicationTypeMismatch)
  return Int
typeCheck env (Mul m n) = do
  t1 <- typeCheck env m
  t2 <- typeCheck env n
  unless (t1 == Int && t2 == Int) (Left ApplicationTypeMismatch)
  return Int
typeCheck env (Div m n) = do
  t1 <- typeCheck env m
  t2 <- typeCheck env n
  unless (t1 == Int && t2 == Int) (Left ApplicationTypeMismatch)
  return Int