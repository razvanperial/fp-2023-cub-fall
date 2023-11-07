{-# LANGUAGE OverloadedStrings #-}

module Test.Parser where 

import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))
import Data.Text (Text, pack)

import Parser
import Syntax

unit_LiteralTests = do
    intLitteralTests
    boolLitteralTests

unit_VarTests = do
    varTests

unit_IfTests = do
    ifTests

unit_AbstractionTests = do
    abstractionTests

unit_ApplicationTests = do
    applicationTests




intLitteralTests = do
    parseLambdaTerm "42" @?= Right (IntLit 42)
    parseLambdaTerm "876510" @?= Right (IntLit (876510))

boolLitteralTests = do
    parseLambdaTerm "True" @?= Right (BoolLit True)
    parseLambdaTerm "False" @?= Right (BoolLit False)

varTests = do
    parseLambdaTerm "x" @?= Right (Var "x")
    parseLambdaTerm "y" @?= Right (Var "y")
    parseLambdaTerm "z" @?= Right (Var "z")

ifTests = do
    parseLambdaTerm "If True Then False Else True" @?= Right (If (BoolLit True) (BoolLit False) (BoolLit True))
    parseLambdaTerm "If True Then If False Then True Else False Else False" @?= Right (If (BoolLit True) (If (BoolLit False) (BoolLit True) (BoolLit False)) (BoolLit False))

abstractionTests = do
    parseLambdaTerm "\\x:Bool. x" @?= Right (Abs "x" Bool (Var "x"))
    parseLambdaTerm "\\x:Bool. \\y:Bool. \\z:Bool. x" @?= Right (Abs "x" Bool (Abs "y" Bool (Abs "z" Bool (Var "x"))))
    parseLambdaTerm "\\x:Bool. \\y:Bool. \\z:Bool. y" @?= Right (Abs "x" Bool (Abs "y" Bool (Abs "z" Bool (Var "y"))))

applicationTests = do
    parseLambdaTerm "(\\x:Bool.x) (y) (z) (w) (v)" @?= Right (App (App (App (App (Abs "x" Bool (Var "x")) (Var "y")) (Var "z")) (Var "w")) (Var "v"))
    parseLambdaTerm "(\\x:Bool.x) (y) (z) (w) (v) (u)" @?= Right (App (App (App (App (App (Abs "x" Bool (Var "x")) (Var "y")) (Var "z")) (Var "w")) (Var "v")) (Var "u"))
    parseLambdaTerm "(\\x:Bool.x) (y) (z) (w) (v) (u) (t)" @?= Right (App (App (App (App (App (App (Abs "x" Bool (Var "x")) (Var "y")) (Var "z")) (Var "w")) (Var "v")) (Var "u")) (Var "t"))