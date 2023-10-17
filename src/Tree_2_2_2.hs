module Tree_2_2_2 where

  data Color = R | B deriving Show
  data Tree a = E | T Color (Tree a) a (Tree a) deriving Show

  -- Check if a value is present in the tree
  member :: (Ord a) => a -> Tree a -> Bool
  member x E = False
  member x (T _ left val right)
    | x < val     = member x left
    | x == val    = True
    | otherwise   = member x right

  -- Adjust tree to maintain Red-Black properties
  adjust :: Color -> Tree a -> a -> Tree a -> Tree a
  adjust B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
  adjust B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
  adjust B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
  adjust B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
  adjust color left val right = T color left val right

  insert :: (Ord a) => a -> Tree a -> Tree a
  insert x s = makeBlack (ins x s)
    where
      ins :: (Ord a) => a -> Tree a -> Tree a
      ins x E = T R E x E -- New node is always red
      ins x s@(T color a y b)
        | x < y     = adjust color (ins x a) y b
        | x == y    = s
        | otherwise = adjust color a y (ins x b)
      
      makeBlack :: Tree a -> Tree a
      makeBlack (T _ a y b) = T B a y b