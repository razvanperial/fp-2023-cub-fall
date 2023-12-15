import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad
import Control.Monad.ST

countingSort :: [Int] -> [Int]
countingSort xs = concatMap (\(n, count) -> replicate count n) $ assocs countArray
  where
    countArray = runSTUArray $ do
      counts <- newArray (0, maximum xs) 0 :: ST s (STUArray s Int Int)
      forM_ xs $ \x -> do
        currentCount <- readArray counts x
        writeArray counts x (currentCount + 1)
      return counts

-- O(n) time
findMissing :: [Int] -> Int
findMissing xs = findMissing' (countingSort xs) 1

findMissing' :: [Int] -> Int -> Int
findMissing' [] n = n
findMissing' (x:xs) n
  | x == n    = findMissing' xs (n + 1)
  | otherwise = n

-- Example usage:
main :: IO ()
main = do
  let inputList = [3, 1, 4, 5, 6]
  putStrLn $ "Input List: " ++ show inputList
  putStrLn $ "Smallest missing number: " ++ show (findMissing inputList)