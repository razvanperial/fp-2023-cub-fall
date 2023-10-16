module SkewBinomialHeapTests where

import Test.Hspec
import SkewBinomialHeap

spec :: Spec
spec = do
  describe "isEmpty" $ do
    it "returns True for an empty heap" $
      isEmpty ([] :: SkewBinomialHeap Int) `shouldBe` True

    it "returns False for a non-empty heap" $
      isEmpty [Node 0 1 [] []] `shouldBe` False

  describe "insert" $ do
    it "inserts an element into an empty heap" $
      insert 1 [] `shouldBe` [Node 0 1 [] []]

    it "inserts an element into a non-empty heap" $
      insert 2 [Node 0 1 [] []] `shouldBe` [Node 0 2 [] [], Node 0 1 [] []]

  describe "insertAll" $ do
    it "inserts a list of elements into an empty heap" $
      insertAll [1, 2] [] `shouldBe` [Node 0 1 [] [], Node 0 2 [] []]

    it "inserts a list of elements into a non-empty heap" $
      insertAll [2, 3, 4] [Node 0 1 [] []] `shouldBe` [Node 0 2 [] [], Node 1 1 [3] [Node 0 4 [] []]]

  describe "findMin" $ do
    it "returns the minimum element in a heap" $
      findMin [Node 0 1 [] [], Node 0 2 [] [], Node 0 3 [] []] `shouldBe` 1

  describe "deleteMin" $ do
    it "deletes the minimum element in a heap" $
      deleteMin [Node 0 1 [] [], Node 0 2 [] [], Node 0 3 [] []] `shouldBe` [Node 1 2 [] [Node 0 3 [] []]]

  describe "toList" $ do
    it "converts a heap to a list" $
      toList [Node 0 1 [] [], Node 0 2 [] [], Node 0 3 [] []] `shouldBe` [1, 2, 3]

  describe "fromList" $ do
    it "converts a list to a heap" $
      fromList [1, 2, 3] `shouldBe` [Node 1 1 [2] [Node 0 3 [] []]]