module Decaf.Data.Tree where

-- | A generic tree
data Tree a = Node a (Maybe [Tree a])
            | Nil
            deriving (Eq, Show)
