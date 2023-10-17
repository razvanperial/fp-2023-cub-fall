module Test.Tree_2_1Tests where

import Test.Tasty
import Test.Tasty.HUnit
import Tree_2_1

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

removeTest :: TestTree
removeTest = testGroup "remove"
  [
    testCase "removes an element from a tree with a single element" $
    member 1 (remove 1 (insert 1 E)) @?= False

    , testCase "removes an element from a tree with multiple elements" $
    member 2 (remove 1 (insert 1 (insert 2 E))) @?= True

    , testCase "removes the root element from a tree with multiple elements" $
    member 2 (remove 1 (insert 1 (insert 2 (insert 3 E)))) @?= True

  ]

visualizeTreeTest :: TestTree
visualizeTreeTest = testGroup "visualizeTree"
  [
    testCase "returns the string representation of an empty tree" $
    visualizeTree (E :: Tree Int) @?= "E"

    , testCase "returns the string representation of a tree with a single element" $
    visualizeTree (insert 1 E) @?= "(B E 1 E)"

    , testCase "returns the string representation of a tree with multiple elements" $
    visualizeTree (insert 2 (insert 1 (insert 3 E))) @?= "(B (B E 1 E) 2 (B E 3 E))"

  ]

units :: [TestTree]
units = [memberTest, insertTest, removeTest, visualizeTreeTest]
