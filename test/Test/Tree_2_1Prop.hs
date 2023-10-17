module Test.Tree_2_1Prop where

    import Hedgehog
    import qualified Hedgehog.Gen as Gen
    import qualified Hedgehog.Range as Range
    import Test.Tasty
    import Test.Tasty.Hedgehog

    import Tree_2_1

    -- Property: Elements not inserted into the tree should not be present
    prop_insertNotMember :: Property
    prop_insertNotMember = property $ do
        xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int Range.linearBounded)
        let tree = fromOrdList xs
        x <- forAll (Gen.int Range.linearBounded)
        if not (x `elem` xs)
            then assert $ not (member x tree)
            else success

    -- Property: Elements removed from the tree should not be present
    prop_removeNotMember :: Property
    prop_removeNotMember = property $ do
        xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int Range.linearBounded)
        let tree = fromOrdList xs
        x <- forAll (Gen.int Range.linearBounded)
        let treeWithoutX = remove x tree
        if x `elem` xs
            then assert $ not (member x treeWithoutX)
            else success

    props :: [TestTree]
    props = [
        testProperty "prop_insertNotMember" prop_insertNotMember
        , testProperty "prop_removeNotMember" prop_removeNotMember
        ]