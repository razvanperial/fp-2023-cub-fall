module BankersDequeProp where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import BankersDeque
import Deque

import Prelude hiding (last)

-- Property tests

-- test that the first element of a deque is the first element pushed
prop_pushFront :: Property
prop_pushFront = property $ do
  x <- forAll $ Gen.int (Range.linear 0 100)
  xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 100))
  first (pushFront x (fromList xs)) === x

-- test that the last element of a deque is the last element pushed
prop_pushBack :: Property
prop_pushBack = property $ do
  x <- forAll $ Gen.int (Range.linear 0 100)
  xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 100))
  last (pushBack (fromList xs) x) === x

-- test the isEmpty function
prop_isEmpty :: Property
prop_isEmpty = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 100))
  let deque = fromList xs
  isEmpty deque === null xs

-- test thet the popBack function removes the last element
prop_popFront :: Property
prop_popFront = property $ do
  xs <- forAll $ Gen.list (Range.linear 1 100) (Gen.int (Range.linear 0 100))
  let deque = fromList xs
  let updatedDeque = popFront deque
  isEmpty updatedDeque === (length xs == 1)


props :: [TestTree]
props = [
    testProperty "prop_pushFront" prop_pushFront
  , testProperty "prop_pushBack" prop_pushBack
  , testProperty "prop_isEmpty" prop_isEmpty
  , testProperty "prop_popFront" prop_popFront
  ]
