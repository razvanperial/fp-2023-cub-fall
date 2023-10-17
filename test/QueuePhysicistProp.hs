module QueuePhysicistProp where

    import Hedgehog
    import qualified Hedgehog.Gen as Gen
    import qualified Hedgehog.Range as Range
    import Test.Tasty
    import Test.Tasty.Hedgehog

    import Queue
    import QueuePhysicist

    import Prelude hiding (head, tail)

    -- Property tests

    -- Verify that the queue is empty if and only if the list is empty.
    prop_isEmpty :: Property
    prop_isEmpty = property $ do
        xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 100))
        let queue = fromList xs
        empty queue === null xs

    --  Verify that enqueue correctly adds an element to the head of the queue 
    -- and that head returns the expected value.
    prop_enqueue :: Property
    prop_enqueue = property $ do
        x <- forAll $ Gen.int (Range.linear 0 100)
        xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 100))
        let queue = constructor :: QueuePhysicist Int
        let updatedQueue = enqueue queue x
        let updatedQueue2 = foldl enqueue updatedQueue xs
        head updatedQueue === head updatedQueue2
        empty updatedQueue === False


    props :: [TestTree]
    props = [
        testProperty "prop_isEmpty" prop_isEmpty
        , testProperty "prop_enqueue" prop_enqueue
      ]


    