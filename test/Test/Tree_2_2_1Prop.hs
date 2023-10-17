module Test.Tree_2_2_1Prop where

    import Hedgehog
    import qualified Hedgehog.Gen as Gen
    import qualified Hedgehog.Range as Range
    import Test.Tasty
    import Test.Tasty.Hedgehog

    import Tree_2_2_1

    -- Property: an element inserted into the tree should be present
    prop_insertMember :: Property
    prop_insertMember = property $ do
        xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 100))
        let tree = foldl (flip insert) E xs
        x <- forAll (Gen.int Range.linearBounded)
        if x `elem` xs
            then assert $ member x tree
            else success


    props :: [TestTree]
    props = [
        testProperty "prop_insertMember" prop_insertMember
        ]