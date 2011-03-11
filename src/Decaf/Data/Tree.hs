module Decaf.Data.Tree where

-- | A generic tree
data Tree a = Node {
  content :: a,
  children :: [Tree a]
  } deriving (Eq)

instance (Show a) => Show (Tree a) where
    show node = display 0 node
        where
            display :: (Show a) => Integer -> Tree a -> String
            display indent node = spaces indent ++ Prelude.show (content node) ++ "\n" ++ concatMap (display $ indent+1) (children node)
            spaces indent = map (\_ -> ' ') [1..3*indent]
