module SBH_BootstrappedTests where

import Test.Hspec
import SBH_Bootstrapped

spec :: Spec
spec = do
  describe "isEmpty" $ do
    it "returns True for an empty heap" $
      isEmpty (Empty :: Heap Int) `shouldBe` True

    it "returns False for a non-empty heap" $
      isEmpty (insert 1 (Empty :: Heap Int)) `shouldBe` False

  describe "insert" $ do
    it "adds an element to an empty heap" $
      findMin (insert 1 (Empty :: Heap Int)) `shouldBe` 1

    it "adds an element to a non-empty heap" $
      findMin (insert 2 (insert 1 (Empty :: Heap Int))) `shouldBe` 1

  describe "merge" $ do
    it "merges two empty heaps" $
      isEmpty (merge (Empty :: Heap Int) (Empty :: Heap Int)) `shouldBe` True

    it "merges an empty heap with a non-empty heap" $
      findMin (merge (Empty :: Heap Int) (insert 1 (Empty :: Heap Int))) `shouldBe` 1

    it "merges two non-empty heaps" $
      findMin (merge (insert 2 (Empty :: Heap Int)) (insert 1 (Empty :: Heap Int))) `shouldBe` 1

    it "removes the minimum element from a heap with multiple elements" $
      findMin (deleteMin (insertAll [2, 1] (Empty :: Heap Int))) `shouldBe` 2

  describe "fromList" $ do
    it "creates an empty heap from an empty list" $
      isEmpty (fromList ([] :: [Int])) `shouldBe` True

    it "creates a heap from a list of elements" $
      findMin (fromList [2, 1] :: Heap Int) `shouldBe` 1

  describe "toList" $ do
    it "returns an empty list for an empty heap" $
      toList (Empty :: Heap Int) `shouldBe` []

    it "returns a list of elements in the heap" $
      toList (insertAll [2, 1] (Empty :: Heap Int)) `shouldBe` [2, 1]