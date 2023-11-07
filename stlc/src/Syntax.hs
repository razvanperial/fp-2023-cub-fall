{-# LANGUAGE FlexibleInstances #-}

module Syntax where

import Text.Printf (printf)
import Data.Char (chr)
import Data.List (nub)
import Data.Maybe (fromJust)
import qualified Data.Set as S

data Type
  = TyVar String
  | Arrow Type Type
  | Bool
  | Int  -- New type for integer expressions
  deriving (Eq, Ord)

instance Show Type where
  show (TyVar v) = v
  show (Arrow (Arrow t1 t2) t3) = printf  "(%s -> %s) -> %s" (show t1) (show t2) (show t3)
  show (Arrow t1 t2) = printf  "%s -> %s" (show t1) (show t2)
  show Bool = "Bool"
  show Int = "Int"  -- Show "Int" for integer expressions

data Term a
  = Var a
  | Abs a Type (Term a)
  | App (Term a) (Term a)
  | BoolLit Bool
  | If (Term a) (Term a) (Term a)
  | Let a (Term a) (Term a)
  | IntLit Int  -- New constructor for integer literals
  | Add (Term a) (Term a)  -- New constructor for addition expressions
  | Sub (Term a) (Term a)  -- New constructor for subtraction expressions
  | Mul (Term a) (Term a)  -- New constructor for multiplication expressions
  | Div (Term a) (Term a)  -- New constructor for division expressions
  deriving (Eq)

instance Show (Term String) where
  show (Var x) = x
  show (Abs x typ t) = printf "λ%s:%s.%s" x (show typ) (show t)
  show (App t1 t2) = printf "(%s) (%s)" (show t1) (show t2)
  show (BoolLit b) = printf "%s" (show b)
  show (If c t e) = printf "if %s then %s else %s" (show c) (show t) (show e)
  show (Let x t1 t2) = printf "let %s = %s in %s" x (show t1) (show t2)
  show (IntLit n) = show n
  show (Add t1 t2) = printf "(%s + %s)" (show t1) (show t2)
  show (Sub t1 t2) = printf "(%s - %s)" (show t1) (show t2)
  show (Mul t1 t2) = printf "(%s * %s)" (show t1) (show t2)
  show (Div t1 t2) = printf "(%s / %s)" (show t1) (show t2)
