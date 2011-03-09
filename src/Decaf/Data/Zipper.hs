module Decaf.Data.Zipper where
import Decaf.Data.Tree

data Context a = Root
               | Child Int (Tree a) (Context a)
               deriving (Eq)

type Zipper a = (Tree a, Context a)

context :: Zipper a -> Context a
context (_, c) = c

top :: Tree a -> Zipper a
top t = (t, Root)

select :: Int -> Zipper a -> Zipper a
select i ((Node val children), ctx)
  = (children !! i, Child i (Node val (take i children ++ drop (i+1) children)) ctx)

parent :: Zipper a -> Zipper a
parent (tree, Child n (Node val children) c)
  = (Node val (take n children ++ [tree] ++ drop n children), c)
parent (_, Root) = error "Zipper.hs:22 Root context has no parent"

root :: Zipper a -> Zipper a
root l@(_, Root) = l
root l = root (parent l)

modify :: (Tree a -> Tree a) -> Zipper a -> Zipper a
modify f (t, c) = (f t, c)

setContext :: Context a -> (Zipper a -> Zipper a)
setContext c = (genPath c) . root

genPath :: Context a -> (Zipper a -> Zipper a)
genPath Root = id
genPath (Child i _ c) = (select i) . (genPath c)

isRoot :: Zipper a -> Bool
isRoot (_, Root) = True
isRoot _ = False

addChild :: a -> Zipper a -> Zipper a
addChild v (Node val children, ctx) = (tree'', ctx')
    where
      newNodes = children ++ [Node v []]
      tree' = Node val newNodes -- insert node
      nodeNumber = length newNodes - 1-- get it's number
      (tree'', ctx') = (select nodeNumber) (tree', ctx) -- focus on the new node

modifyContent :: (a -> a) -> Zipper a -> Zipper a
modifyContent f = modify changecontent
    where changecontent (Node val children) = Node (f val) children

getContent :: Zipper a -> a
getContent (Node val _, _) = val

mkZipper :: a -> Zipper a
mkZipper val = top (Node val [])

generify :: (Show a) => Zipper a -> Tree String
generify z = let (tree, _) = root z
             in rewrite tree
  where
    rewrite :: (Show a) => Tree a -> Tree String
    rewrite (Node val children) = Node (show val) (map rewrite children)

instance (Show a) => Show (Context a) where
    show Root = "ROOT"
    show (Child i _ c) = "CHILD " ++ Prelude.show i ++ " <" ++ show c ++ ">"
