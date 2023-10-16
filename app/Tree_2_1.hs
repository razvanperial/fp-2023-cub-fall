module Tree_2_1 where

  data Color = R | B deriving Show
  data Tree a = E | T Color (Tree a) a (Tree a) deriving Show

  -- Check if a value is present in the tree
  member :: (Ord a) => a -> Tree a -> Bool
  member x E = False
  member x (T _ left val right)
    | x < val     = member x left
    | x == val    = True
    | otherwise   = member x right

  -- Add a value to the tree
  insert :: (Ord a) => a -> Tree a -> Tree a
  insert x tree = ensureRootIsBlack $ ins tree
    where 
      ins E = T R E x E
      ins (T color left val right)
        | x < val  = adjust color (ins left) val right
        | x == val = T color left val right
        | x > val  = adjust color left val (ins right)
      ensureRootIsBlack (T _ left val right) = T B left val right

  -- Adjust tree to maintain Red-Black properties
  adjust :: Color -> Tree a -> a -> Tree a -> Tree a
  adjust B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
  adjust B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
  adjust B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
  adjust B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
  adjust color left val right = T color left val right

  -- Optional: Adjust tree without color parameter
  rebalance :: Tree a -> Tree a
  rebalance (T color left val right) = adjust color left val right

  -- Remove a value from the tree
  remove :: (Ord a) => a -> Tree a -> Tree a
  remove x tree = ensureRootIsBlack $ del x tree
    where 
      ensureRootIsBlack (T _ left val right) = T B left val right
      ensureRootIsBlack E = E

  del :: (Ord a) => a -> Tree a -> Tree a
  del x t@(T _ l y r)
    | x < y = delL x t
    | x > y = delR x t
    | otherwise = fuse l r

  delL :: (Ord a) => a -> Tree a -> Tree a
  delL x t@(T B t1 y t2) = balL $ T B (del x t1) y t2
  delL x t@(T R t1 y t2) = T R (del x t1) y t2

  balL :: Tree a -> Tree a
  balL (T B (T R t1 x t2) y t3) = T R (T B t1 x t2) y t3
  balL (T B t1 y (T B t2 z t3)) = rebalance (T B t1 y (T R t2 z t3))
  balL (T B t1 y (T R (T B t2 u t3) z t4@(T B l value r))) =
    T R (T B t1 y t2) u (rebalance (T B t3 z (T R l value r)))

  delR :: (Ord a) => a -> Tree a -> Tree a
  delR x t@(T B t1 y t2) = balR $ T B t1 y (del x t2)
  delR x t@(T R t1 y t2) = T R t1 y (del x t2)

  balR :: Tree a -> Tree a
  balR (T B t1 y (T R t2 x t3)) = T R t1 y (T B t2 x t3)
  balR (T B (T B t1 z t2) y t3) = rebalance (T B (T R t1 z t2) y t3)
  balR (T B (T R t1@(T B l value r) z (T B t2 u t3)) y t4) =
    T R (rebalance (T B (T R l value r) z t2)) u (T B t3 y t4)

  fuse :: Tree a -> Tree a -> Tree a
  fuse E t = t
  fuse t E = t
  fuse t1@(T B _ _ _) (T R t3 y t4) = T R (fuse t1 t3) y t4
  fuse (T R t1 x t2) t3@(T B _ _ _) = T R t1 x (fuse t2 t3)
  fuse (T R t1 x t2) (T R t3 y t4)  =
    let s = fuse t2 t3
    in case s of
        (T R s1 z s2) -> (T R (T R t1 x s1) z (T R s2 y t4))
        (T B _ _ _)   -> (T R t1 x (T R s y t4))
  fuse (T B t1 x t2) (T B t3 y t4)  =
    let s = fuse t2 t3
    in case s of
        (T R s1 z s2) -> (T R (T B t1 x s1) z (T B s2 y t4))
        (T B s1 z s2) -> balL (T B t1 x (T B s y t4))

  -- Convert tree to string for visualization
  visualizeTree :: (Show a) => Tree a -> String
  visualizeTree E = "E"
  visualizeTree (T color left val right) =
    "(" ++ show color ++ " " ++
    visualizeTree left ++ " " ++
    show val ++ " " ++
    visualizeTree right ++ ")"

  -- Construct a tree from a list of ordered values
  fromOrdList :: (Ord a) => [a] -> Tree a
  fromOrdList xs = foldl (\tree x -> insert x tree) E xs

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

