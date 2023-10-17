module QueueRealTime where

    import Queue

    data QueueRealTime a = RTQ {
        f :: [a],
        r :: [a],
        s :: [a]
    } deriving (Show)

    rotate :: [a] -> [a] -> [a] -> [a]
    rotate [] [r] s = r:s
    rotate (f:fs) (r:rs) s = f: rotate fs rs (r:s)

    invariant :: QueueRealTime a -> QueueRealTime a
    invariant (RTQ f r []) =
        let f' = rotate f r []
        in RTQ f' [] f'
    invariant (RTQ f r (x:s)) = RTQ f r s

    fromList :: [a] -> QueueRealTime a
    fromList xs = RTQ xs [] xs


    instance Queue QueueRealTime where

        constructor = RTQ [] [] []

        empty (RTQ [] _ _) = True
        empty _              = False

        enqueue (RTQ f r s) x = invariant $ RTQ f (x:r) s

        head (RTQ [] _ _) = error "Queue is empty"
        head q@(RTQ {f = (f:_)}) = f

        tail (RTQ [] [] []) = error "Queue is empty"
        tail q@(RTQ {f = (_:f)}) = invariant $ q {f = f}