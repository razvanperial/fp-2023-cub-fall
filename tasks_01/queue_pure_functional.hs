import Criterion.Main

data Queue a = Queue [a] [a] deriving (Show)

empty :: Queue a
empty = Queue [] []

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue f r) = Queue f (x:r)

head :: Queue a -> Maybe a
head (Queue [] _) = Nothing
head (Queue (x:_) _) = Just x

tail :: Queue a -> Queue a
tail (Queue [] _) = empty
tail (Queue (_:f) r) = Queue f r

-- Sample Data
sampleQueue1 :: Queue Int
sampleQueue1 = foldl (flip enqueue) empty [1..10]

sampleQueue2 :: Queue Int
sampleQueue2 = foldl (flip enqueue) empty [1..100]

sampleQueue3 :: Queue Int
sampleQueue3 = foldl (flip enqueue) empty [1..1000]

-- Benchmarks
main :: IO ()
main = defaultMain
  [ bench "enqueue 42 into sampleQueue1" $ whnf (\n -> enqueue n sampleQueue1) 42
  , bench "enqueue 42 into sampleQueue2" $ whnf (\n -> enqueue n sampleQueue2) 42
  , bench "enqueue 42 into sampleQueue3" $ whnf (\n -> enqueue n sampleQueue3) 42
  , bench "dequeue from sampleQueue1" $ whnf Main.tail sampleQueue1
  , bench "dequeue from sampleQueue2" $ whnf Main.tail sampleQueue2
  , bench "dequeue from sampleQueue3" $ whnf Main.tail sampleQueue3
  , bench "front of sampleQueue1" $ whnf Main.head sampleQueue1
  , bench "front of sampleQueue2" $ whnf Main.head sampleQueue2
  , bench "front of sampleQueue3" $ whnf Main.head sampleQueue3
  ]