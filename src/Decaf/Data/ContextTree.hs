module Decaf.Data.ContextTree where

data ContextTree a = ContextTree {
    node :: Node a,
    context :: Context
    }

data Node a = Node {
    content :: a,
    children :: [Node a]
    }

type Context = [Int]

mkContextTree :: a -> ContextTree a
mkContextTree content = ContextTree (Node content []) []

move :: (Context -> Context) -> ContextTree a -> ContextTree a
move f t = ContextTree (node t) (f . context $ t)

parent :: ContextTree a -> ContextTree a
parent = move init

setContext :: Context -> ContextTree a -> ContextTree a
setContext c = move (\_ -> c)

root :: ContextTree a -> ContextTree a
root = move (\_ -> [])

isRoot :: ContextTree a -> Bool
isRoot (ContextTree _ c) = null c

-- applies f to the current context (considered as a tree) and returns modified t
modify :: (ContextTree a -> ContextTree a) -> ContextTree a -> ContextTree a
modify f t = if isRoot t -- this is the node to be modified
             then f t  -- modify it
             else let ctxt = context t -- otherwise recurse
                      pos = head ctxt
                      root = node t -- current location
                      nodes = children root
                      branch = modify f (ContextTree (nodes !! pos) (tail ctxt))
                  in ContextTree (Node (content root)
                        (take pos nodes
                         ++ [node branch] -- insert the modified subtree in the right place
                         ++ drop (pos+1) nodes)) (pos : context branch)

addChild :: a -> ContextTree a -> ContextTree a
addChild val tree = modify addchild tree
          where addchild (ContextTree node _) = let newNodes = children node ++ [Node val []]
                                                in ContextTree (Node (content node) newNodes) [length newNodes -1]

modifyContextTreeContent :: (a -> a) -> ContextTree a -> ContextTree a
modifyContextTreeContent f = modify g
          where g (ContextTree node cnt) = ContextTree (Node (f (content node)) (children node)) cnt

getContent :: ContextTree a -> a
getContent t = fetch (node t) (context t)
          where fetch :: Node a -> Context -> a
                fetch n [] = content n
                fetch n (i:rest) = fetch (children n !! i) rest

instance (Show a) => Show (Node a) where
    show node = 
        display 0 node
        where
            display :: (Show a) => Integer -> Node a -> String
            display indent node = spaces indent ++ Prelude.show (content node) ++ "\n" ++ concatMap (display $ indent+1) (children node)
            spaces indent = map (\_ -> ' ') [1..2*indent]

instance (Show a) => Show (ContextTree a) where
    show t = show (node t) ++ show (context t)
