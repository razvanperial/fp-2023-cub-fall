{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Test.Matrix.Prop where

import Matrix
import QuadTree
import Test.Matrix.Gen
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- Property-based test to ensure that inserting a value into a quad tree at a specific point
-- results in the expected value retrieval at that point in the modified quad tree
hprop_Insert :: Property
hprop_Insert = property $ do
  -- Generate a random size for the quad tree (within the range [1, 10])
  size <- forAll $ Gen.int (Range.constant 1 10) -- Adjust the range as needed
  -- Generate a random quad tree of the specified size with random integer values (within the range [-100, 100])
  quadTree <- forAll $ genMatrix size 100
  -- Generate random coordinates (x, y) within the bounds of the quad tree.
  x <- forAll $ Gen.int (Range.constant 0 (2^(size - 1) - 1))
  y <- forAll $ Gen.int (Range.constant 0 (2^(size - 1) - 1))
  -- Generate a random value to insert into the quad tree (within the range [0, 100]).
  val <- forAll $ Gen.int (Range.constant 0 100) -- Adjust the range as needed

  -- Create a point with the generated coordinates.
  let point = Point x y
      modifiedQuadTree = insert quadTree point val

  -- Check that the value retrieved from the inserted point in the modified quad tree
  -- is equal to the value that was inserted.
  getValueAtPoint modifiedQuadTree point === Just val

-- Helper function to access the matrix at a specific point
getValueAtPoint :: Eq a => SquareQuadTree a -> Point -> Maybe a
getValueAtPoint (Cell _ x) _ = Just x
getValueAtPoint (Quad size nw ne sw se) point =
  let Point px py = point in
  case getTrail size point of
    []      -> Nothing
    NW:trail -> getValueAtPoint nw (Point px py)
    NE:trail -> getValueAtPoint ne (Point px py)
    SW:trail -> getValueAtPoint sw (Point px py)
    SE:trail -> getValueAtPoint se (Point px py)


-- hprop_unitAdd :: Property
-- hprop_unitAdd = property $ do
--   let zeroQuadTree b = Cell b 0 
--   b <- forAll $ genNat 5
--   m <- forAll $ genMatrix b 10
--   zeroQuadTree b === m `mult` zeroQuadTree b
--   zeroQuadTree b === zeroQuadTree b `mult` m

-- hprop_commAdd :: Property
-- hprop_commAdd = property $ do 
--   b <- forAll $ genNat 5 
--   m1 <- forAll $ genMatrix b 10 
--   m2 <- forAll $ genMatrix b 10 
--   m1 `add` m2 === m2 `add` m1

-- hprop_subNeg :: Property
-- hprop_subNeg = property $ do 
--   b <- forAll $ genNat 5 
--   m1 <- forAll $ genMatrix b 10 
--   m2 <- forAll $ genMatrix b 10 
--   m1 `add` neg m2 === m1 `sub` m2

-- hprop_scalarMultDistr :: Property
-- hprop_scalarMultDistr = property $ do 
--   b <- forAll $ genNat 5 
--   m1 <- forAll $ genMatrix b 10 
--   m2 <- forAll $ genMatrix b 10 
--   c1 <- forAll $ genInt 10 
--   c2 <- forAll $ genInt 10
--   c1 `scalarMult` (m1 `add` m2) === (c1 `scalarMult` m1) `add` (c1 `scalarMult` m2)
--   (c1 + c2) `scalarMult` m1 === (c1 `scalarMult` m1) `add` (c2 `scalarMult` m1)

-- hprop_unitMult :: Property
-- hprop_unitMult = property $ do 
--   b <- forAll $ genNat 5 
--   m <- forAll $ genMatrix b 10 
--   let unit = diagonalM b 1  
--   m === m `mult` unit
--   m === unit `mult` m
 
-- hprop_assocMult :: Property
-- hprop_assocMult = property $ do
--   b <- forAll $ genNat 5
--   m1 <- forAll $ genMatrix b 10
--   m2 <- forAll $ genMatrix b 10
--   m3 <- forAll $ genMatrix b 10
--   (m1 `mult` m2) `mult` m3 === m1 `mult` (m2 `mult` m3)

-- hprop_addTranspose :: Property
-- hprop_addTranspose = property $ do 
--   b <- forAll $ genNat 5 
--   m1 <- forAll $ genMatrix b 10
--   m2 <- forAll $ genMatrix b 10
--   transpose (m1 `add` m2) === transpose m1 `add` transpose m2

-- hprop_multTranspose :: Property
-- hprop_multTranspose = property $ do 
--   b <- forAll $ genNat 5 
--   m1 <- forAll $ genMatrix b 10
--   m2 <- forAll $ genMatrix b 10
--   transpose (m1 `mult` m2) === transpose m2 `mult` transpose m1

-- hprop_scalarMultTranspose :: Property
-- hprop_scalarMultTranspose = property $ do 
--   b <- forAll $ genNat 5 
--   m <- forAll $ genMatrix b 10
--   c <- forAll $ genInt 10
--   transpose (c `scalarMult` m) === c `scalarMult` transpose m
