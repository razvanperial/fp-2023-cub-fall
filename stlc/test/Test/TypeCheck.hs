module Test.TypeCheck where

import Test.Tasty.HUnit (Assertion, (@?=))

import Syntax
import TypeCheck

check :: Term String -> Either String Type -> Assertion
check term expectedType =
  typeCheckEmpty term @?= expectedType

unit_typecheck = do
  check (Abs "x" Bool (Var "x")) (Right (Arrow Bool Bool))
  check (If (App (Abs "x" Bool (Var "x")) (BoolLit True)) (BoolLit True) (BoolLit False)) (Right Bool)
  check
    (Abs "f" (Arrow (TyVar "b") (TyVar "c"))
         (Abs "g" (Arrow (TyVar "a") (TyVar "b"))
              (Abs "x" (TyVar "a") (App (Var "f") (App (Var "g") (Var "x"))))))
    (Right (Arrow (Arrow (TyVar "b") (TyVar "c")) (Arrow (Arrow (TyVar "a") (TyVar "b")) (Arrow (TyVar "a") (TyVar "c")))))
  check (Let "x" (BoolLit True) (If (Var "x") (BoolLit True) (BoolLit False))) (Right Bool)
