import Criterion.Main

data RealTimeQueue a = RTQ {
    f :: [a],  -- Front list
    r :: [a],  -- Reversed list
    s :: [a]   -- Schedule (suffix such that all nodes before s in f have already been forced and memoized)
} deriving (Show)

-- Create an empty queue
empty :: RealTimeQueue a
empty = RTQ [] [] []

-- Check if the queue is empty
isEmpty :: RealTimeQueue a -> Bool
isEmpty (RTQ [] _ _) = True
isEmpty _            = False

-- Rotate function
rotate :: [a] -> [a] -> [a] -> [a]
rotate [] [y] a = y:a
rotate (x:xs) (y:ys) a = x : rotate xs ys (y:a)

-- Ensure the invariants hold
inv :: RealTimeQueue a -> RealTimeQueue a
inv (RTQ f r (_:s)) = RTQ f r s
inv (RTQ f r []) = 
    let f' = rotate f r []
    in RTQ f' [] f'

-- Enqueue an element to the queue
enqueue :: a -> RealTimeQueue a -> RealTimeQueue a
enqueue x q@(RTQ f r s) = inv $ RTQ f (x:r) s

-- Dequeue an element from the queue
dequeue :: RealTimeQueue a -> Maybe (a, RealTimeQueue a)
dequeue (RTQ [] _ _)       = Nothing
dequeue (RTQ (x:f) r s)    = Just (x, inv $ RTQ f r s)

-- Peek the front element of the queue
front :: RealTimeQueue a -> Maybe a
front (RTQ [] _ _)      = Nothing
front (RTQ (x:_) _ _)   = Just x


-- Sample Data
sampleQueue1 :: RealTimeQueue Int
sampleQueue1 = foldl (flip enqueue) empty [1..10]

sampleQueue2 :: RealTimeQueue Int
sampleQueue2 = foldl (flip enqueue) empty [1..100]

sampleQueue3 :: RealTimeQueue Int
sampleQueue3 = foldl (flip enqueue) empty [1..1000]

-- Benchmarks
main :: IO ()
main = defaultMain
  [ bench "enqueue 42 into sampleQueue1" $ whnf (\n -> enqueue n sampleQueue1) 42
  , bench "enqueue 42 into sampleQueue2" $ whnf (\n -> enqueue n sampleQueue2) 42
  , bench "enqueue 42 into sampleQueue3" $ whnf (\n -> enqueue n sampleQueue3) 42
  , bench "dequeue from sampleQueue1" $ whnf dequeue sampleQueue1
  , bench "dequeue from sampleQueue2" $ whnf dequeue sampleQueue2
  , bench "dequeue from sampleQueue3" $ whnf dequeue sampleQueue3
  , bench "front of sampleQueue1" $ whnf front sampleQueue1
  , bench "front of sampleQueue2" $ whnf front sampleQueue2
  , bench "front of sampleQueue3" $ whnf front sampleQueue3
  ]

