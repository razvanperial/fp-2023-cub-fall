module Deque where

    class Deque deque where

        constructor :: deque a
        isEmpty :: deque a -> Bool

        pushFront :: a -> deque a -> deque a
        first :: deque a -> a
        popFront :: deque a -> deque a

        pushBack :: deque a -> a -> deque a
        last :: deque a -> a
        popBack :: deque a -> deque a