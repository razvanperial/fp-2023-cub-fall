module SkewBinomialHeap where

    data Tree a = Node {
      rank :: Int,
      root :: a,
      singletons :: [a],
      children :: [Tree a] 
    } deriving (Show, Eq, Ord)

    type SkewBinomialHeap a = [Tree a]

    isEmpty :: SkewBinomialHeap a -> Bool
    isEmpty [] = True
    isEmpty _ = False

    link :: Ord a => Tree a -> Tree a -> Tree a
    link s1@(Node r x xs c) s2@(Node _ y ys d)
      | x <= y    = s1 { rank = r + 1, children = s2 : c }
      | otherwise = s2 { rank = r + 1, children = s1 : d }

    skewLink :: Ord a => a -> Tree a -> Tree a -> Tree a
    skewLink x s1 s2 = 
      let Node r y ys c = link s1 s2
      in if x <= y then Node r x (y:ys) c else Node r y (x:ys) c

    insert :: Ord a => a -> SkewBinomialHeap a -> SkewBinomialHeap a
    insert x [] = [Node 0 x [] []]
    insert x [t] = (Node 0 x [] []) : [t]
    insert x ts@(t1:t2:rs)
      | rank t1 == rank t2  = skewLink x t1 t2 : rs
      | otherwise           = Node 0 x [] [] : ts

    insTree :: Ord a => Tree a -> SkewBinomialHeap a -> SkewBinomialHeap a
    insTree t [] = [t]
    insTree t ts@(t':ts')
      | rank t < rank t' = t : ts
      | otherwise        = insTree (link t t') ts'

    insertAll :: Ord a => [a] -> SkewBinomialHeap a -> SkewBinomialHeap a
    insertAll xs ts = foldr insert ts xs

    findMin :: Ord a => SkewBinomialHeap a -> a
    findMin [] = error "Empty heap"
    findMin [t] = root t
    findMin (t:ts) = let x = root t
                         y = findMin ts
                     in if x <= y then x else y

    norm :: Ord a => SkewBinomialHeap a -> SkewBinomialHeap a
    norm [] = []
    norm [t] = [t]
    norm (t:ts) = insTree t ts

    mergeNormalised :: Ord a => SkewBinomialHeap a -> SkewBinomialHeap a -> SkewBinomialHeap a
    mergeNormalised [] ts = ts
    mergeNormalised ts [] = ts
    mergeNormalised ts1@(t1:ts1') ts2@(t2:ts2')
      | rank t1 < rank t2 = t1 : mergeNormalised ts1' ts2
      | rank t2 < rank t1 = t2 : mergeNormalised ts1 ts2'
      | otherwise         = insTree (link t1 t2) (mergeNormalised ts1' ts2')


    merge :: Ord a => SkewBinomialHeap a -> SkewBinomialHeap a -> SkewBinomialHeap a
    merge [] ts = ts
    merge ts [] = ts
    merge ts1 ts2 = let ts1' = norm ts1
                        ts2' = norm ts2
                    in mergeNormalised ts1' ts2'

    deleteMin :: Ord a => SkewBinomialHeap a -> SkewBinomialHeap a
    deleteMin ts = let getMin [t] = (t, [])
                       getMin (t:ts) = let (t', ts') = getMin ts
                                       in if root t <= root t' then (t, ts) else (t', t:ts')
                       (Node _ x xs c, ts') = getMin ts
                    in insertAll xs (merge (reverse c) (norm ts'))

    toList :: Ord a => SkewBinomialHeap a -> [a]
    toList [] = []
    toList [t] = root t : singletons t
    toList (t:ts) = root t : singletons t ++ toList ts

    fromList :: Ord a => [a] -> SkewBinomialHeap a
    fromList xs = insertAll xs []