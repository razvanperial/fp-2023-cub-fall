module QueuePhysicist where

    import Queue

    data QueuePhysicist a = PQ {
        wList :: [a],
        frontList :: [a],
        frontSize :: Int,
        rearList :: [a],
        rearSize :: Int
    } deriving (Show)

    checkW :: QueuePhysicist a -> QueuePhysicist a
    checkW q@(PQ w f lenF r lenR)
        | null w    = PQ f f lenF r lenR
        | otherwise = PQ w f lenF r lenR

    checkR :: QueuePhysicist a -> QueuePhysicist a
    checkR q@(PQ w f lenF r lenR)
        | lenR <= lenF = q
        | otherwise    = PQ f (f ++ reverse r) (lenF + lenR) [] 0

    invariant :: QueuePhysicist a -> QueuePhysicist a
    invariant q = checkW $ checkR q

    instance Queue QueuePhysicist where

        constructor = PQ [] [] 0 [] 0

        empty (PQ [] [] 0 [] 0) = True
        empty _                 = False

        enqueue (PQ w f lenF r lenR) x = invariant $ PQ w f lenF (x:r) (lenR + 1)

        head (PQ [] [] _ [] _) = error "Queue is empty"
        head (PQ (x:_) _ _ _ _) = x

        tail (PQ [] [] _ [] _) = error "Queue is empty"
        tail (PQ (_:w) (_:f) lenF r lenR) = invariant $ PQ w f (lenF - 1) r lenR
