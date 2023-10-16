module QueueBankers where

    import Queue

    data QueueBankers a = BQ {
        frontList :: [a],
        rearList :: [a],
        frontSize :: Int,
        rearSize :: Int
    } deriving (Show)

    invariant :: QueueBankers a -> QueueBankers a
    invariant q@(BQ frontList rearList frontSize rearSize)
        | frontSize >= rearSize = q
        | otherwise             = BQ (frontList ++ reverse rearList) [] (frontSize + rearSize) 0

    instance Queue QueueBankers where

        constructor = BQ [] [] 0 0

        empty (BQ [] [] 0 0) = True
        empty _              = False

        enqueue (BQ frontList rearList frontSize rearSize) x = invariant $ BQ frontList (x:rearList) frontSize (rearSize + 1)

        head (BQ [] [] _ _) = error "Queue is empty"
        head (BQ (x:_) _ _ _) = x

        tail (BQ [] [] _ _) = error "Queue is empty"
        tail (BQ (_:frontList) rearList frontSize rearSize) = invariant $ BQ frontList rearList (frontSize - 1) rearSize
