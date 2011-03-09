module Decaf.Data.Zipper where

data Tree a = Node {
    content :: a,
    children ::[Tree a]
  } deriving (Eq)

data Context a = Root
               | Child Int (Tree a) (Context a)
               deriving (Eq)

type Zipper a = (Tree a, Context a)

select :: Int -> Zipper a -> Zipper a
select i (t@(Node val children), ctx) = (children !! i, Child i t ctx)

top :: Tree a -> Zipper a
top t = (t, Root)

parent :: Zipper a -> Zipper a
parent (tree, Child n (Node val children) c) = (Node val (take n children ++ [tree] ++ drop n children), c)

root :: Zipper a -> Zipper a
root l@(t, Root) = l
root l = root (parent l)

modify :: (Tree a -> Tree a) -> Zipper a -> Zipper a
modify f (t, c) = (f t, c)

-- implement
--setContext :: Context -> ContextTree a -> ContextTree a
--setContext c = move (\_ -> c)

isRoot :: Zipper a -> Bool
isRoot (_, Root) = True
isRoot _ = False

addChild :: a -> Zipper a -> Zipper a
addChild v z = modify addchild z
    where addchild (Node val children) = Node val (children ++ [Node v []])

modifyContent :: (a -> a) -> Zipper a -> Zipper a
modifyContent f = modify changecontent
    where changecontent (Node val children) = Node (f val) children

getContent :: Zipper a -> a
getContent (Node val children, _) = val

mkZipper :: a -> Zipper a
mkZipper val = top (Node val [])

instance (Show a) => Show (Tree a) where
    show node = display 0 node
        where
            display indent node = spaces indent ++ Prelude.show (content node) ++ "\n" ++ concatMap (display $ indent+1) (children node)
            spaces indent = map (\_ -> ' ') [1..3*indent]

instance (Show a) => Show (Context a) where
    show Root = "ROOT"
    show (Child i t c) = "CHILD " ++ Prelude.show i ++ " <" ++ show c ++ ">"
