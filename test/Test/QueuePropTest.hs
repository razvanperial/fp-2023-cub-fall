module Test.QueuePropTest where

    import Queue

    import Hedgehog
    import qualified Hedgehog.Gen as Gen
    import qualified Hedgehog.Range as Range
    import Test.Tasty
    import Test.Tasty.Hedgehog

    import Queue
    import QueueBankers
    import QueuePhysicist
    import QueueRealTime
    import QueuePureFunctional

    import Prelude hiding (head, tail)
    
    genInt :: Gen Int
    genInt = Gen.int (Range.linear 0 100)

    genIntList :: Gen [Int]
    genIntList = Gen.list (Range.linear 0 100) genInt

    -- Property tests

    prop_isEmpty :: Queue q => q Int -> Property
    prop_isEmpty q = property $ do
        xs <- forAll genIntList
        let queue = foldl enqueue q xs
        empty queue === null xs

    prop_enqueue :: Queue q => q Int -> Property
    prop_enqueue q = property $ do
        x <- forAll genInt
        xs <- forAll genIntList
        let queue = enqueue q x
        let updatedQueue = foldl enqueue queue xs
        head updatedQueue === x
        empty updatedQueue === False

    bankers_queue :: [TestTree]
    bankers_queue = [
        testProperty "prop_isEmpty" (prop_isEmpty (constructor :: QueueBankers Int))
        , testProperty "prop_enqueue" (prop_enqueue (constructor :: QueueBankers Int))
      ]
    
    physicist_queue :: [TestTree]
    physicist_queue = [
        testProperty "prop_isEmpty" (prop_isEmpty (constructor :: QueuePhysicist Int))
        , testProperty "prop_enqueue" (prop_enqueue (constructor :: QueuePhysicist Int))
      ]
    
    real_time_queue :: [TestTree]
    real_time_queue = [
        testProperty "prop_isEmpty" (prop_isEmpty (constructor :: QueueRealTime Int))
        , testProperty "prop_enqueue" (prop_enqueue (constructor :: QueueRealTime Int))
      ]

    pure_functional_queue :: [TestTree]
    pure_functional_queue = [
        testProperty "prop_isEmpty" (prop_isEmpty (constructor :: QueuePureFunctional Int))
        , testProperty "prop_enqueue" (prop_enqueue (constructor :: QueuePureFunctional Int))
      ]





