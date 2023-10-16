module Queue where

    class Queue queue where
        constructor :: queue a
        empty :: queue a -> Bool
        enqueue :: queue a -> a -> queue a
        head :: queue a -> a
        tail :: queue a -> queue a