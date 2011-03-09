module Decaf.Data.Zipper where

data Tree a = Node a [Tree a]
            deriving (Eq, Show)

data Context a = Root
               | Child Int (Tree a) (Context a)
               deriving (Eq, Show)

type Location a = (Tree a, Context a)

select :: Int -> Location a -> Location a
select i (t@(Node val children), ctx) = (children !! i, Child i t ctx)

top :: Tree a -> Location a
top t = (t, Root)

up :: Location a -> Location a
up (tree, (Child n (Node val children) c)) = (Node val (take n children ++ [tree] ++ drop n children), c)

upmost :: Location a -> Location a
upmost l@(t, Root) = l
upmost l = upmost (up l)

modify :: (Tree a -> Tree a) -> Location a -> Location a
modify f (t, c) = (f t, c)
