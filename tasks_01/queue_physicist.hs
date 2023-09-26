import Criterion.Main

data PhysicistsQueue a = PQ {
    w    :: [a],
    f    :: [a],
    lenF :: Int,
    r    :: [a],
    lenR :: Int
} deriving (Show)

-- Create an empty queue
empty :: PhysicistsQueue a
empty = PQ [] [] 0 [] 0

-- Check if the queue is empty
isEmpty :: PhysicistsQueue a -> Bool
isEmpty (PQ [] _ _ _ _) = True
isEmpty _               = False

-- Enqueue an element to the queue
enqueue :: a -> PhysicistsQueue a -> PhysicistsQueue a
enqueue x (PQ w f lenF r lenR) = invQueue $ PQ w f lenF (x:r) (lenR + 1)

-- Dequeue an element from the queue
dequeue :: PhysicistsQueue a -> Maybe (a, PhysicistsQueue a)
dequeue (PQ [] _ _ _ _)       = Nothing
dequeue (PQ (x:w) f lenF r lenR) = Just (x, invQueue $ PQ w (tail f) (lenF - 1) r lenR)

-- Peek the front element of the queue
front :: PhysicistsQueue a -> Maybe a
front (PQ [] _ _ _ _)      = Nothing
front (PQ (x:_) _ _ _ _)   = Just x

-- Ensure the invariants hold
invQueue :: PhysicistsQueue a -> PhysicistsQueue a
invQueue q@(PQ [] f lenF r lenR) = 
    let w' = force f
    in PQ w' f lenF r lenR
invQueue q@(PQ w f lenF r lenR)
    | lenR <= lenF = q
    | otherwise    = 
        let w' = force f
        in PQ w' (w' ++ reverse r) (lenF + lenR) [] 0

-- Helper function to force the evaluation of a list
force :: [a] -> [a]
force xs = foldr seq xs xs

-- Sample Data
sampleQueue1 :: PhysicistsQueue Int
sampleQueue1 = foldl (flip enqueue) empty [1..10]

sampleQueue2 :: PhysicistsQueue Int
sampleQueue2 = foldl (flip enqueue) empty [1..100]

sampleQueue3 :: PhysicistsQueue Int
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

