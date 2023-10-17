module QueuePureFunctionalProp where

    import Hedgehog
    import qualified Hedgehog.Gen as Gen
    import qualified Hedgehog.Range as Range
    import Test.Tasty
    import Test.Tasty.Hedgehog

    import Queue
    import QueuePureFunctional

    import Prelude hiding (head, tail)

    prop_isEmpty :: Property
    prop_isEmpty = property $ do
        xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 100))
        let queue = fromList xs
        empty queue === null xs

    prop_enqueue :: Property
    prop_enqueue = property $ do
        x <- forAll $ Gen.int (Range.linear 0 100)
        xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 100))
        let queue = enqueue ( constructor :: QueuePureFunctional Int ) x
        let updatedQueue = foldl enqueue queue xs
        head updatedQueue === x
        empty updatedQueue === False

    props :: [TestTree]
    props = [
        testProperty "prop_isEmpty" prop_isEmpty
        , testProperty "prop_enqueue" prop_enqueue
      ]

