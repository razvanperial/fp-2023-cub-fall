module QueueBankersProp where

    import Hedgehog
    import qualified Hedgehog.Gen as Gen
    import qualified Hedgehog.Range as Range
    import Test.Tasty
    import Test.Tasty.Hedgehog

    import Queue
    import QueueBankers

    import Prelude hiding (head, tail)

    -- Property tests

    -- test that the first element of a queue is the first element pushed
    prop_enqueue :: Property
    prop_enqueue = property $ do
        x <- forAll $ Gen.int (Range.linear 0 100)
        xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 100))
        let q = constructor :: QueueBankers Int
        let q' = enqueue q x
        let q'' = foldl enqueue q' xs
        head q'' === x

    -- enqueueing and dequeueing to an empty queue should return the same element
    prop_enqueue_dequeue :: Property
    prop_enqueue_dequeue = property $ do
        x <- forAll $ Gen.int (Range.linear 0 100)
        let q = constructor :: QueueBankers Int
        let q' = enqueue q x
        head q' === x


    props :: [TestTree]
    props = [
        testProperty "prop_enqueue" prop_enqueue
        , testProperty "prop_enqueue_dequeue" prop_enqueue_dequeue
      ]
