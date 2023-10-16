module Main where

    import Prelude hiding (head, tail)

    import Queue
    import QueueBankers (QueueBankers)
    import QueuePureFunctional (QueuePureFunctional)
    import QueueRealTime (QueueRealTime)
    import QueuePhysicist (QueuePhysicist)
    
    import Criterion.Main
    
    main :: IO ()
    main = defaultMain [
        bgroup "QueueBankers" [
            bench "enqueue" $ whnf (foldl enqueue (constructor :: QueueBankers Int)) ([1..20000] :: [Int]),
            bench "tail" $ whnf (foldl (\q _ -> tail q) (foldl enqueue (constructor :: QueueBankers Int) ([1..20000] :: [Int]))) ([1..20000] :: [Int])
        ],
        bgroup "QueuePhysicist" [
            bench "enqueue" $ whnf (foldl enqueue (constructor :: QueuePhysicist Int)) ([1..20000] :: [Int]),
            bench "tail" $ whnf (foldl (\q _ -> tail q) (foldl enqueue (constructor :: QueuePhysicist Int) ([1..20000] :: [Int]))) ([1..20000] :: [Int])
        ],
        bgroup "QueuePureFunctional" [
            bench "enqueue" $ whnf (foldl enqueue (constructor :: QueuePureFunctional Int)) ([1..20000] :: [Int]),
            bench "tail" $ whnf (foldl (\q _ -> tail q) (foldl enqueue (constructor :: QueuePureFunctional Int) ([1..20000] :: [Int]))) ([1..20000] :: [Int])
        ],
        bgroup "QueueRealTime" [
            bench "enqueue" $ whnf (foldl enqueue (constructor :: QueueRealTime Int)) ([1..20000] :: [Int]),
            bench "tail" $ whnf (foldl (\q _ -> tail q) (foldl enqueue (constructor :: QueueRealTime Int) ([1..20000] :: [Int]))) ([1..19999] :: [Int])
        ]
        ]