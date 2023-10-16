module SBH_Bootstrapped where

    import qualified SkewBinomialHeap as SBH

    data Heap a = Empty | NonEmpty (SBH.SkewBinomialHeap a) deriving (Show, Eq, Ord)

    isEmpty :: Ord a => Heap a -> Bool
    isEmpty Empty = True
    isEmpty _ = False

    merge :: Ord a => Heap a -> Heap a -> Heap a
    merge Empty h = h
    merge h Empty = h
    merge (NonEmpty h1) (NonEmpty h2) = NonEmpty (SBH.merge h1 h2)

    insert :: Ord a => a -> Heap a -> Heap a
    insert x Empty = NonEmpty (SBH.insert x [])
    insert x (NonEmpty h) = NonEmpty (SBH.insert x h)

    insertAll :: Ord a => [a] -> Heap a -> Heap a
    insertAll xs h = foldr insert h xs

    findMin :: Ord a => Heap a -> a
    findMin Empty = error "Empty heap"
    findMin (NonEmpty h) = SBH.findMin h

    deleteMin :: Ord a => Heap a -> Heap a
    deleteMin Empty = error "Empty heap"
    deleteMin (NonEmpty h) = NonEmpty (SBH.deleteMin h)

    fromList :: Ord a => [a] -> Heap a
    fromList xs = insertAll xs Empty

    toList :: Ord a => Heap a -> [a]
    toList Empty = []
    toList (NonEmpty h) = SBH.toList h

    sort :: Ord a => [a] -> [a]
    sort xs = toList (fromList xs)