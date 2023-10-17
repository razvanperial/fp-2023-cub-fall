module Test.SBH_BootstrappedTests where

import Test.Tasty
import Test.Tasty.HUnit
import SBH_Bootstrapped

isEmptyTest :: TestTree
isEmptyTest = testGroup "isEmpty"
  [
    testCase "returns True for an empty heap" $
    isEmpty (Empty :: Heap Int) @?= True

    , testCase "returns False for a non-empty heap" $
    isEmpty (insert 1 (Empty :: Heap Int)) @?= False

  ]

insertTest :: TestTree
insertTest = testGroup "insert"
  [
    testCase "adds an element to an empty heap" $
    findMin (insert 1 (Empty :: Heap Int)) @?= 1

    , testCase "adds an element to a non-empty heap" $
    findMin (insert 2 (insert 1 (Empty :: Heap Int))) @?= 1

  ]

mergeTest :: TestTree
mergeTest = testGroup "merge"
  [
    testCase "merges two empty heaps" $
    isEmpty (merge (Empty :: Heap Int) (Empty :: Heap Int)) @?= True

    , testCase "merges an empty heap with a non-empty heap" $
    findMin (merge (Empty :: Heap Int) (insert 1 (Empty :: Heap Int))) @?= 1

    , testCase "merges two non-empty heaps" $
    findMin (merge (insert 2 (Empty :: Heap Int)) (insert 1 (Empty :: Heap Int))) @?= 1

    , testCase "removes the minimum element from a heap with multiple elements" $
    findMin (deleteMin (insertAll [2, 1] (Empty :: Heap Int))) @?= 2

  ]

fromListTest :: TestTree
fromListTest = testGroup "fromList"
  [
    testCase "creates an empty heap from an empty list" $
    isEmpty (fromList ([] :: [Int])) @?= True

    , testCase "creates a heap from a list of elements" $
    findMin (fromList [2, 1] :: Heap Int) @?= 1

  ]

toListTest :: TestTree
toListTest = testGroup "toList"
  [
    testCase "returns an empty list for an empty heap" $
    toList (Empty :: Heap Int) @?= []

    , testCase "returns a list of elements in the heap" $
    toList (insertAll [2, 1] (Empty :: Heap Int)) @?= [2, 1]

  ]

units :: [TestTree]
units = [isEmptyTest, insertTest, mergeTest, fromListTest, toListTest]