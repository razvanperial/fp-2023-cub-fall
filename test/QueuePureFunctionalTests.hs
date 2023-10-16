module QueuePureFunctionalTests where

import Test.Hspec
import QueuePureFunctional
import Queue

import Prelude hiding (head, tail)

spec :: Spec
spec = do
  describe "empty" $ do
    it "returns True for an empty queue" $
      empty (constructor :: QueuePureFunctional Int) `shouldBe` True

    it "returns False for a non-empty queue" $
      empty (enqueue (constructor :: QueuePureFunctional Int) 1) `shouldBe` False

  describe "enqueue" $ do
    it "adds an element to an empty queue" $
      head (enqueue (constructor :: QueuePureFunctional Int) 1) `shouldBe` 1

    it "adds an element to a non-empty queue" $
      head (enqueue (enqueue (constructor :: QueuePureFunctional Int) 1) 2) `shouldBe` 1

  describe "head" $ do
    it "returns the first element of a non-empty queue" $
      head (enqueue (constructor :: QueuePureFunctional Int) 1) `shouldBe` 1

  describe "tail" $ do
    it "removes the first element of a queue with one element" $
      empty (tail (enqueue (constructor :: QueuePureFunctional Int) 1)) `shouldBe` True

    it "removes the first element of a queue with multiple elements" $
      head (tail (enqueue (enqueue (constructor :: QueuePureFunctional Int) 1) 2)) `shouldBe` 2