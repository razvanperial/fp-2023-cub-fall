module BankersDeque where
    
    import Deque

    data BankersDeque a = BD {
        front :: [a],
        rear :: [a],
        lenf :: Int,
        lenr :: Int
    } deriving (Show, Eq)

    -- c is the constant that determines the maximum ratio between the sizes of the front and rear lists
    c = 2

    checkInvariant :: BankersDeque a -> BankersDeque a
    checkInvariant q@(BD front rear lenf lenr)
        | lenf > c * lenr + 1 = let i = (lenf + lenr) `div` 2
                                    j = lenf + lenr - i
                                    f' = take i front
                                    r' = rear ++ reverse (drop i front)
                                in BD f' r' i j
        | lenr > c * lenf + 1 = let j = (lenf + lenr) `div` 2
                                    i = lenf + lenr - j
                                    r' = take j rear
                                    f' = front ++ reverse (drop j rear)
                                in BD f' r' i j
        | otherwise = q

    fromList :: [a] -> BankersDeque a
    fromList xs = BD xs [] (length xs) 0

    instance Deque BankersDeque where
        constructor = BD [] [] 0 0

        isEmpty (BD front rear lenf lenr) = lenf + lenr == 0

        pushFront x (BD front rear lenf lenr) = checkInvariant $ BD (x:front) rear (lenf + 1) lenr

        first (BD [] _ _ _) = error "Deque is empty"
        first (BD (x:_) _ _ _) = x

        popFront (BD [] _ _ _) = error "Deque is empty"
        popFront (BD (x:front) rear lenf lenr) = checkInvariant $ BD front rear (lenf - 1) lenr

        pushBack (BD front rear lenf lenr) x = checkInvariant $ BD front (x:rear) lenf (lenr + 1)

        last (BD _ [] _ _) = error "Deque is empty"
        last (BD _ (x:_) _ _) = x
        
        popBack (BD _ [] _ _) = error "Deque is empty"
        popBack (BD front (_:rear) lenf lenr) = checkInvariant $ BD front rear lenf (lenr - 1)
        
