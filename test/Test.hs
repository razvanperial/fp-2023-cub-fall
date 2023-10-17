import Test.Tasty 
import BankersDequeProp
import QueueBankersProp
import QueuePhysicistProp
import QueuePureFunctionalProp
import QueueRealTimeProp
import SBH_BootstrappedProp
import SkewBinomialHeapProp

main :: IO ()

main = do
    print "Running tests..."
    defaultMain (testGroup "All tests"
        [ 
            testGroup "Property testing BankersDeque" BankersDequeProp.props
            , testGroup "Property testing QueueBankers" QueueBankersProp.props
            , testGroup "Property testing QueuePhysicist" QueuePhysicistProp.props
            , testGroup "Property testing QueuePureFunctional" QueuePureFunctionalProp.props
            , testGroup "Property testing QueueRealTime" QueueRealTimeProp.props
            , testGroup "Property testing SBH_Bootstrapped" SBH_BootstrappedProp.props
            , testGroup "Property testing SkewBinomialHeap" SkewBinomialHeapProp.props
        ])