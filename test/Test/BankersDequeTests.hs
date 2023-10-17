module Test.BankersDequeTests where

import Test.Tasty
import Test.Tasty.HUnit
import BankersDeque
import Deque

import Prelude hiding (last)

-- Unit tests
isEmptyTest :: TestTree
isEmptyTest = testGroup "isEmpty"
  [ testCase "returns True for an empty deque" $
    isEmpty (constructor :: BankersDeque Int) @?= True

    , testCase "returns False for a non-empty deque" $
    isEmpty (pushFront 1 (constructor :: BankersDeque Int)) @?= False

  ]

pushFrontTest :: TestTree
pushFrontTest = testGroup "pushFront"
  [ testCase "adds an element to the front of an empty deque" $
    first (pushFront 1 (constructor :: BankersDeque Int)) @?= 1

    , testCase "adds an element to the front of a non-empty deque" $
    first (pushFront 2 (pushFront 1 (constructor :: BankersDeque Int))) @?= 2

  ]

pushBackTest :: TestTree
pushBackTest = testGroup "pushBack"
  [ testCase "adds an element to the back of an empty deque" $
    last (pushBack (constructor :: BankersDeque Int) 1) @?= 1

    , testCase "adds an element to the back of a non-empty deque" $
    last (pushBack (pushBack (constructor :: BankersDeque Int) 1) 2) @?= 2

  ]

popFrontTest :: TestTree
popFrontTest = testGroup "popFront"
  [ testCase "removes an element from the front of a deque with one element" $
    isEmpty (popFront (pushFront 1 (constructor :: BankersDeque Int))) @?= True

    , testCase "removes an element from the front of a deque with multiple elements" $
    last (popFront (pushFront 2 (pushFront 1 (constructor :: BankersDeque Int)))) @?= 1

  ]

popBackTest :: TestTree
popBackTest = testGroup "popBack"
  [ testCase "removes an element from the back of a deque with one element" $
    isEmpty (popBack (pushBack (constructor :: BankersDeque Int) 1)) @?= True

    , testCase "removes an element from the back of a deque with multiple elements" $
    first (popBack (pushBack (pushBack (constructor :: BankersDeque Int) 1) 2)) @?= 1

  ]

units :: [TestTree]
units = [
  isEmptyTest
  , pushFrontTest
  , pushBackTest
  , popFrontTest
  , popBackTest
  ]
  