module QueuePureFunctional where
    
    import Queue

    data QueuePureFunctional a = QPF {
        frontList :: [a],
        rearList :: [a]
    } deriving (Show)

    invariant :: QueuePureFunctional a -> QueuePureFunctional a
    invariant q@(QPF frontList rearList)
        | null frontList  = QPF (reverse rearList) []
        | otherwise       = q

    instance Queue QueuePureFunctional where

        constructor = QPF [] []

        empty (QPF [] []) = True
        empty _           = False

        enqueue (QPF frontList rearList) x = invariant $ QPF frontList (x:rearList)

        head (QPF [] []) = error "Queue is empty"
        head (QPF (x:_) _) = x

        tail (QPF [] []) = error "Queue is empty"
        tail (QPF (_:frontList) rearList) = invariant $ QPF frontList rearList