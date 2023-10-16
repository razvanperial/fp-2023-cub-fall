module Main where

    import qualified BankersDeque as BD
    -- import qualified SkewBinomialHeap as SBH
    import Deque
    -- import Dijkstra

    import Prelude hiding (last)

    main :: IO ()

    main = do
        -- BankersDeque
        let q = constructor :: BD.BankersDeque Int
        let q' = pushFront 1 q
        let q'' = pushFront 2 q'
        let q''' = pushFront 3 q''
        let q'''' = pushBack q''' 4
        let q''''' = pushBack q'''' 5
        let q'''''' = pushBack q''''' 6
        let q''''''' = pushBack q'''''' 7
        let q'''''''' = pushBack q''''''' 8
        print $ last q''''''''
        print $ first q''''''''
        print $ popBack q''''''''
        print $ popFront q''''''''

        -- Dijkstra





