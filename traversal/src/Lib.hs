{-# LANGUAGE InstanceSigs #-}
module Lib
    ( someFunc
    ) where

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show, Eq)

instance Functor Tree where 
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Leaf x) = Leaf $ f x 
    fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where 
    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap f (Leaf x) = f x 
    foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r

instance Traversable Tree where 
    traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse f (Leaf x) = Leaf <$> f x 
    traverse f (Node l x r) = Node <$> traverse f l <*> f x <*> traverse f r 
    
tree = Node (Node (Leaf 1) 2 (Node (Leaf 3) 4 (Leaf 5))) 6 (Node (Leaf 7) 8 (Leaf 9))

someFunc :: IO ()
someFunc = do 
    print tree 
    putStrLn "====================="
    print ((+2) <$> tree)
    print (foldMap (\x -> [x]) tree) 
    print (traverse (\x -> if even x then Right x else Left x) ((2*) <$> tree))
