module Test.SkewBinomialHeapTests where

import Test.Tasty
import Test.Tasty.HUnit
import SkewBinomialHeap

isEmptyTest :: TestTree
isEmptyTest = testGroup "isEmpty"
  [
    testCase "returns True for an empty heap" $
    isEmpty ([] :: SkewBinomialHeap Int) @?= True

    , testCase "returns False for a non-empty heap" $
    isEmpty ([Node 0 1 [] []] :: SkewBinomialHeap Int) @?= False

  ]

insertTest :: TestTree
insertTest = testGroup "insert"
  [
    testCase "inserts an element into an empty heap" $
    insert 1 [] @?= [Node 0 1 [] []]

    , testCase "inserts an element into a non-empty heap" $
    insert 2 [Node 0 1 [] []] @?= [Node 0 2 [] [], Node 0 1 [] []]

  ]

insertAllTest :: TestTree
insertAllTest = testGroup "insertAll"
  [
    testCase "inserts a list of elements into an empty heap" $
    insertAll [1, 2] [] @?= [Node 0 1 [] [], Node 0 2 [] []]

    , testCase "inserts a list of elements into a non-empty heap" $
    insertAll [2, 3, 4] [Node 0 1 [] []] @?= [Node 0 2 [] [], Node 1 1 [3] [Node 0 4 [] []]]

  ]

findMinTest :: TestTree
findMinTest = testGroup "findMin"
  [
    testCase "returns the minimum element in a heap" $
    findMin [Node 0 1 [] [], Node 0 2 [] [], Node 0 3 [] []] @?= 1

  ]
  
deleteMinTest :: TestTree
deleteMinTest = testGroup "deleteMin"
  [
    testCase "deletes the minimum element in a heap" $
    deleteMin [Node 0 1 [] [], Node 0 2 [] [], Node 0 3 [] []] @?= [Node 1 2 [] [Node 0 3 [] []]]

  ]

toListTest :: TestTree
toListTest = testGroup "toList"
  [
    testCase "converts a heap to a list" $
    toList [Node 0 1 [] [], Node 0 2 [] [], Node 0 3 [] []] @?= [1, 2, 3]

  ]
  
fromListTest :: TestTree
fromListTest = testGroup "fromList"
  [
    testCase "converts a list to a heap" $
    fromList [1, 2, 3] @?= [Node 1 1 [2] [Node 0 3 [] []]]

  ]

units :: [TestTree]
units = [isEmptyTest, insertTest, insertAllTest, findMinTest, deleteMinTest, toListTest, fromListTest]