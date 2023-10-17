module Test.Tree_2_2_2Tests where

import Test.Tasty
import Test.Tasty.HUnit
import Tree_2_2_2


memberTest :: TestTree
memberTest = testGroup "member"
  [
    testCase "returns False for an empty tree" $
    member 1 (E :: Tree Int) @?= False

    , testCase "returns True for a tree with a single element" $
    member 1 (insert 1 E) @?= True

    , testCase "returns True for a tree with multiple elements" $
    member 3 (insert 2 (insert 1 (insert 3 E))) @?= True

    , testCase "returns False for a tree without the searched element" $
    member 4 (insert 2 (insert 1 (insert 3 E))) @?= False

  ]

insertTest :: TestTree
insertTest = testGroup "insert"
  [
    testCase "adds an element to an empty tree" $
    member 1 (insert 1 E) @?= True

    , testCase "adds an element to a non-empty tree" $
    member 2 (insert 1 (insert 2 E)) @?= True

  ]

units :: [TestTree]
units = [
    memberTest
    , insertTest
  ]