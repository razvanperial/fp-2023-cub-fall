module BankersDequeTests where

import Test.Hspec
import BankersDeque
import Deque

import Prelude hiding (last)

spec :: Spec
spec = do
  describe "isEmpty" $ do
    it "returns True for an empty deque" $
      isEmpty (constructor :: BankersDeque Int) `shouldBe` True

    it "returns False for a non-empty deque" $
      isEmpty (pushFront 1 (constructor :: BankersDeque Int)) `shouldBe` False

  describe "pushFront" $ do
    it "adds an element to the front of an empty deque" $
      first (pushFront 1 (constructor :: BankersDeque Int)) `shouldBe` 1

    it "adds an element to the front of a non-empty deque" $
      first (pushFront 2 (pushFront 1 (constructor :: BankersDeque Int))) `shouldBe` 2

  describe "pushBack" $ do
    it "adds an element to the back of an empty deque" $
      last (pushBack (constructor :: BankersDeque Int) 1) `shouldBe` 1

    it "adds an element to the back of a non-empty deque" $
      last (pushBack (pushBack (constructor :: BankersDeque Int) 1) 2) `shouldBe` 2

  describe "popFront" $ do
    it "removes an element from the front of a deque with one element" $
      isEmpty (popFront (pushFront 1 (constructor :: BankersDeque Int))) `shouldBe` True

    it "removes an element from the front of a deque with multiple elements" $
      last (popFront (pushFront 2 (pushFront 1 (constructor :: BankersDeque Int)))) `shouldBe` 1

  describe "popBack" $ do
    it "removes an element from the back of a deque with one element" $
      isEmpty (popBack (pushBack (constructor :: BankersDeque Int) 1)) `shouldBe` True

    it "removes an element from the back of a deque with multiple elements" $
      first (popBack (pushBack (pushBack (constructor :: BankersDeque Int) 1) 2)) `shouldBe` 1