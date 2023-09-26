import Criterion.Main

data Color = R | B deriving Show
data Tree a = E | T Color (Tree a) a (Tree a) deriving Show

-- Check if a value is present in the tree
member :: (Ord a) => a -> Tree a -> Bool
member x E = False
member x (T _ left val right)
  | x < val     = member x left
  | x == val    = True
  | otherwise   = member x right

insert :: (Ord a) => a -> Tree a -> Tree a
insert x s = makeBlack $ ins s
  where ins E  = T R E x E
        ins (T color a y b)
          | x < y  = rbalance color (ins a) y b
          | x == y = T color a y b
          | x > y  = lbalance color a y (ins b)
        makeBlack (T _ a y b) = T B a y b

lbalance :: Color -> Tree a -> a -> Tree a -> Tree a
lbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
lbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
lbalance B (T R a x b) y c = T R (T B a x b) y (T B c z d)
  where T B c z d = c
lbalance color a x b = T color a x b

rbalance :: Color -> Tree a -> a -> Tree a -> Tree a
rbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
rbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
rbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
rbalance color a x b = T color a x b

-- Convert tree to string for visualization
visualizeTree :: (Show a) => Tree a -> String
visualizeTree E = "E"
visualizeTree (T color left val right) =
  "(" ++ show color ++ " " ++
  visualizeTree left ++ " " ++
  show val ++ " " ++
  visualizeTree right ++ ")"

-- Sample Data
sampleTreeX :: Tree Int
sampleTreeX = T B E 45 E

sampleTreeY :: Tree Int
sampleTreeY =
  T B
    (T R (E) 7 (E))
    14
    (T R (E) 21 (T B (E) 28 (E)))

sampleTreeZ :: Tree Int
sampleTreeZ =
  T B
    (T R (T B (E) 3 (E)) 6 (E))
    12
    (T R (T B (E) 89 (E)) 99 (E))


-- Benchmarks
main :: IO ()
main = defaultMain
  [ bench "insert 100 into sampleTreeX" $ whnf (\n -> insert n sampleTreeX) 100
  , bench "insert 200 into sampleTreeX" $ whnf (\n -> insert n sampleTreeX) 200
  , bench "insert 7 into sampleTreeX" $ whnf (\n -> insert n sampleTreeX) 7
  , bench "insert 15 into sampleTreeY" $ whnf (\n -> insert n sampleTreeY) 15
  , bench "insert 22 into sampleTreeY" $ whnf (\n -> insert n sampleTreeY) 22
  , bench "insert 29 into sampleTreeY" $ whnf (\n -> insert n sampleTreeY) 29
  , bench "insert 4 into sampleTreeZ" $ whnf (\n -> insert n sampleTreeZ) 4
  , bench "insert 19 into sampleTreeZ" $ whnf (\n -> insert n sampleTreeZ) 19
  , bench "insert 35 into sampleTreeZ" $ whnf (\n -> insert n sampleTreeZ) 35
  ]