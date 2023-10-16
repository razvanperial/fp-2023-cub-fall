module Main where

    import SkewBinomialHeapTests
    import BankersDequeTests
    import SBH_BootstrappedTests
    import QueueBankersTests
    import QueuePhysicistTests
    import QueuePureFunctionalTests
    import QueueRealTimeTests

    import Prelude hiding (last)
    import Test.Hspec

    main :: IO ()

    main = hspec $ do
        SkewBinomialHeapTests.spec
        BankersDequeTests.spec
        SBH_BootstrappedTests.spec
        QueueBankersTests.spec
        QueuePhysicistTests.spec
        QueuePureFunctionalTests.spec
        QueueRealTimeTests.spec
        -- Dijkstra





