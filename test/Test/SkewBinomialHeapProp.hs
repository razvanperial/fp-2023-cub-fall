module Test.SkewBinomialHeapProp where

    import Hedgehog
    import qualified Hedgehog.Gen as Gen
    import qualified Hedgehog.Range as Range
    import Test.Tasty
    import Test.Tasty.Hedgehog
    import qualified Data.Set as Set

    import SkewBinomialHeap

    prop_isEmpty :: Property
    prop_isEmpty = property $ do
        xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 100))
        let heap = fromList xs
        isEmpty heap === null xs

    prop_insert_findMin :: Property
    prop_insert_findMin = property $ do
        x <- forAll $ Gen.int (Range.linear 0 100)
        y <- forAll $ Gen.int (Range.linear 0 100)
        let heap = insert x []
        let updatedHeap = insert y heap
        findMin updatedHeap === min y (findMin heap)

    prop_deleteMin :: Property
    prop_deleteMin = property $ do
        -- ensure all elements are distinct
        xs' <- forAll $ Gen.set (Range.linear 3 100) (Gen.int (Range.linear 3 100))
        let xs = Set.toList xs'
        let heap = fromList xs
        let updatedHeap = deleteMin heap
        findMin updatedHeap === minimum (tail xs)
    
    props :: [TestTree]
    props = [
        testProperty "prop_isEmpty" prop_isEmpty
        , testProperty "prop_insert_findMin" prop_insert_findMin
        , testProperty "prop_deleteMin" prop_deleteMin
        ]
