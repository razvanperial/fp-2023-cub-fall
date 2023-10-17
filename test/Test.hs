import Test.Tasty 
import Test.BankersDequeProp
import Test.QueueBankersProp
import Test.QueuePhysicistProp
import Test.QueuePureFunctionalProp
import Test.QueueRealTimeProp
import Test.SBH_BootstrappedProp
import Test.SkewBinomialHeapProp
import Test.Tree_2_1Prop
import Test.Tree_2_2_1Prop
import Test.Tree_2_2_2Prop
import Test.BankersDequeTests
import Test.QueueBankersTests
import Test.QueuePhysicistTests
import Test.QueuePureFunctionalTests
import Test.QueueRealTimeTests
import Test.SBH_BootstrappedTests
import Test.SkewBinomialHeapTests
import Test.Tree_2_1Tests
import Test.Tree_2_2_1Tests
import Test.Tree_2_2_2Tests

main :: IO ()

main = do
    defaultMain (testGroup "All tests"
        [   testGroup "Property tests"
        [
            testGroup "Property testing BankersDeque" Test.BankersDequeProp.props
            , testGroup "Property testing QueueBankers" Test.QueueBankersProp.props
            , testGroup "Property testing QueuePhysicist" Test.QueuePhysicistProp.props
            , testGroup "Property testing QueuePureFunctional" Test.QueuePureFunctionalProp.props
            , testGroup "Property testing QueueRealTime" Test.QueueRealTimeProp.props
            , testGroup "Property testing SBH_Bootstrapped" Test.SBH_BootstrappedProp.props
            , testGroup "Property testing SkewBinomialHeap" Test.SkewBinomialHeapProp.props
            , testGroup "Property testing Tree_2_1" Test.Tree_2_1Prop.props
            , testGroup "Property testing Tree_2_2_1" Test.Tree_2_2_1Prop.props
            , testGroup "Property testing Tree_2_2_2" Test.Tree_2_2_2Prop.props
        ],
        testGroup "Unit tests"
        [
            testGroup "Unit testing BankersDeque" Test.BankersDequeTests.units
            , testGroup "Unit testing QueueBankers" Test.QueueBankersTests.units
            , testGroup "Unit testing QueuePhysicist" Test.QueuePhysicistTests.units
            , testGroup "Unit testing QueuePureFunctional" Test.QueuePureFunctionalTests.units
            , testGroup "Unit testing QueueRealTime" Test.QueueRealTimeTests.units
            , testGroup "Unit testing SBH_Bootstrapped" Test.SBH_BootstrappedTests.units
            , testGroup "Unit testing SkewBinomialHeap" Test.SkewBinomialHeapTests.units
            , testGroup "Unit testing Tree_2_1" Test.Tree_2_1Tests.units
            , testGroup "Unit testing Tree_2_2_1" Test.Tree_2_2_1Tests.units
            , testGroup "Unit testing Tree_2_2_2" Test.Tree_2_2_2Tests.units
        ]
        ])