{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Heap where

data Tree a = Nil | Node (Tree a) a (Tree a) 
    deriving (Show, Ord, Eq)

class Ord a => Heap a where
    create :: [a] -> Tree a
    add :: a -> Tree a -> Tree a
    count :: Tree a -> Int
    inorder  :: Tree a -> [a]

instance (Ord a, Eq a) => Heap a where
    add newValue Nil = Node Nil newValue Nil
    add newValue (Node left value right)
        | newValue  > value = Node left value (add newValue right)
        | newValue  < value = Node (add newValue left) value right
        | otherwise = Node left value right

    create nodes = create' Nil nodes where
        create' tree []       = tree
        create' tree (x : xs) = create' (add x tree) xs 

    count tree = count' 0 tree where
        count' val Nil = 1
        count' val (Node left _ right) = count' (val + 1) left + count' (val + 1) right

    inorder Nil                     = []
    inorder (Node left value right) = inorder left ++ [value] ++ inorder right

big   = [1000000, 999999..1]
small = [100, 99..1]
t_s   = create small
t_b   = create big