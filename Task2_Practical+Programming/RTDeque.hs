data RealTimeDeque a = RTDeque [a] [a]

emptyDeque :: RealTimeDeque a
emptyDeque = RTDeque [] []

isEmpty :: RealTimeDeque a -> Bool
isEmpty (RTDeque front back) = null front && null back

-- | Push element to the front of the deque
pushFront :: a -> RealTimeDeque a -> RealTimeDeque a
pushFront x (RTDeque front back) = RTDeque (x : front) back

-- | Pop element from the front of the deque
popFront :: RealTimeDeque a -> Maybe (a, RealTimeDeque a)
popFront (RTDeque [] []) = Nothing
popFront (RTDeque [] back) = popFront (RTDeque (reverse back) [])
popFront (RTDeque (x:xs) back) = Just (x, RTDeque xs back)

-- | Push element to the back of the deque
pushBack :: a -> RealTimeDeque a -> RealTimeDeque a
pushBack x (RTDeque front back) = RTDeque front (x : back)

-- | Pop element from the back of the deque
popBack :: RealTimeDeque a -> Maybe (a, RealTimeDeque a)
popBack (RTDeque [] []) = Nothing
popBack (RTDeque front []) = popBack (RTDeque [] (reverse front))
popBack (RTDeque front (x:xs)) = Just (x, RTDeque front xs)

main :: IO ()
main = do
  let empty = emptyDeque
      deque1 = pushFront 1 empty
      deque2 = pushBack 2 deque1
      deque3 = pushFront 0 deque2

  putStrLn $ "Is deque3 empty? " ++ show (isEmpty deque3)

  case popFront deque3 of
    Nothing -> putStrLn "Deque is empty"
    Just (x, newDeque) -> do
      putStrLn $ "Front element: " ++ show x
      putStrLn $ "Is newDeque empty? " ++ show (isEmpty newDeque)
