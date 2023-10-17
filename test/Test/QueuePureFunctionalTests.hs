module Test.QueuePureFunctionalTests where

import Test.Tasty
import Test.Tasty.HUnit
import QueuePureFunctional
import Queue

import Prelude hiding (head, tail)

emptyTest :: TestTree
emptyTest = testGroup "empty"
  [
    testCase "returns True for an empty queue" $
    empty (constructor :: QueuePureFunctional Int) @?= True

    , testCase "returns False for a non-empty queue" $
    empty (enqueue (constructor :: QueuePureFunctional Int) 1) @?= False

  ]

enqueueTest :: TestTree
enqueueTest = testGroup "enqueue"
  [
    testCase "adds an element to an empty queue" $
    head (enqueue (constructor :: QueuePureFunctional Int) 1) @?= 1

    , testCase "adds an element to a non-empty queue" $
    head (enqueue (enqueue (constructor :: QueuePureFunctional Int) 1) 2) @?= 1

  ]

headTest :: TestTree
headTest = testGroup "head"
  [
    testCase "returns the first element of a non-empty queue" $
    head (enqueue (constructor :: QueuePureFunctional Int) 1) @?= 1

  ]

tailTest :: TestTree
tailTest = testGroup "tail"
  [
    testCase "removes the first element of a non-empty queue" $
    head (tail (enqueue (enqueue (constructor :: QueuePureFunctional Int) 1) 2)) @?= 2

    , testCase "removes the first element of a queue with one element" $
    empty (tail (enqueue (constructor :: QueuePureFunctional Int) 1)) @?= True

  ]

units :: [TestTree]
units = [emptyTest, enqueueTest, headTest, tailTest]