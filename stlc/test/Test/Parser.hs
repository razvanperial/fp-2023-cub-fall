{-# LANGUAGE OverloadedStrings #-}

module Test.Parser where 

    import Test.Tasty
    import Test.Tasty.HUnit (testCase, (@?=))
    import Data.Text (Text, pack)

    import Parser
    import Syntax

    literalTests = do
        testIntLit
        testBoolLit

    varTests = do
        testVar

    ifTests = do
        testIf

    abstractionTests = do
        testAbstraction

    applicationTests = do
        testApplication

    testIntLit = do
        parseLambdaTerm "3" @?= Right (IntLit 3)
        parseLambdaTerm "0" @?= Right (IntLit 0)
        parseLambdaTerm "123" @?= Right (IntLit 123)

    testBoolLit = do
        parseLambdaTerm "True" @?= Right (BoolLit True)
        parseLambdaTerm "False" @?= Right (BoolLit False)

    testVar = do
        parseLambdaTerm "x" @?= Right (Var "x")
        parseLambdaTerm "y" @?= Right (Var "y")
        parseLambdaTerm "z" @?= Right (Var "z")

    testIf = do
        parseLambdaTerm "If True Then False Else True" @?= Right (If (BoolLit True) (BoolLit False) (BoolLit True))
        parseLambdaTerm "If True Then If False Then True Else False Else False" @?= Right (If (BoolLit True) (If (BoolLit False) (BoolLit True) (BoolLit False)) (BoolLit False))

    testAbstraction = do
        parseLambdaTerm "//x:Bool. x" @?= Right (Abs "x" Bool (Var "x"))
        parseLambdaTerm "//x:Bool. If x Then False Else True" @?= Right (Abs "x" Bool (If (Var "x") (BoolLit False) (BoolLit True)))
        parseLambdaTerm "//x:Bool. λy:Bool. If x Then y Else False" @?= Right (Abs "x" Bool (Abs "y" Bool (If (Var "x") (Var "y") (BoolLit False))))

    testApplication = do
        parseLambdaTerm "x y" @?= Right (App (Var "x") (Var "y"))
        parseLambdaTerm "x y z" @?= Right (App (App (Var "x") (Var "y")) (Var "z"))
        parseLambdaTerm "x (y z)" @?= Right (App (Var "x") (App (Var "y") (Var "z")))
        parseLambdaTerm "x (y z) w" @?= Right (App (App (Var "x") (App (Var "y") (Var "z"))) (Var "w"))
        parseLambdaTerm "(x y) (z w)" @?= Right (App (App (Var "x") (Var "y")) (App (Var "z") (Var "w")))
        parseLambdaTerm "x (y (z w))" @?= Right (App (Var "x") (App (Var "y") (App (Var "z") (Var "w"))))