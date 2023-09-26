import Criterion.Main

data BankersQueue a = BQ {
    frontList :: [a],
    rearList  :: [a]
} deriving (Show)

-- Create an empty queue
empty :: BankersQueue a
empty = BQ [] []

-- Check if the queue is empty
isEmpty :: BankersQueue a -> Bool
isEmpty (BQ [] []) = True
isEmpty _          = False

-- Enqueue an element to the queue
enqueue :: a -> BankersQueue a -> BankersQueue a
enqueue x (BQ front rear) = checkInvariant $ BQ front (x:rear)

-- Dequeue an element from the queue
dequeue :: BankersQueue a -> Maybe (a, BankersQueue a)
dequeue (BQ [] [])       = Nothing
dequeue (BQ (x:front) rear) = Just (x, checkInvariant $ BQ front rear)

-- Peek the front element of the queue
front :: BankersQueue a -> Maybe a
front (BQ [] [])      = Nothing
front (BQ (x:_) _)    = Just x

-- Ensure the invariant holds: length of front list >= length of rear list
checkInvariant :: BankersQueue a -> BankersQueue a
checkInvariant q@(BQ front rear)
    | length front >= length rear = q
    | otherwise                   = BQ (front ++ reverse rear) []

-- Sample Data
sampleQueue1 :: BankersQueue Int
sampleQueue1 = foldl (flip enqueue) empty [1..10]

sampleQueue2 :: BankersQueue Int
sampleQueue2 = foldl (flip enqueue) empty [1..100]

sampleQueue3 :: BankersQueue Int
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