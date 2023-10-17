module Test.Tree_2_2_2Prop where

    import Hedgehog
    import qualified Hedgehog.Gen as Gen
    import qualified Hedgehog.Range as Range
    import Test.Tasty
    import Test.Tasty.Hedgehog

    import Tree_2_2_2 

    -- Property: An element inserted into the tree should be present
    prop_insertMember :: Property
    prop_insertMember = property $ do
        xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int Range.linearBounded)
        let tree = foldl (flip insert) E xs
        x <- forAll (Gen.int Range.linearBounded)
        if x `elem` xs
            then assert $ member x tree
            else success

    -- Property: An element not inserted into the tree should not be present
    prop_insertNotMember :: Property
    prop_insertNotMember = property $ do
        xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int Range.linearBounded)
        let tree = foldl (flip insert) E xs
        x <- forAll (Gen.int Range.linearBounded)
        if not (x `elem` xs)
            then assert $ not (member x tree)
            else success

    props :: [TestTree]
    props = [
        testProperty "prop_insertMember" prop_insertMember
        , testProperty "prop_insertNotMember" prop_insertNotMember
        ]

    
