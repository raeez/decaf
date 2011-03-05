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
move f t = ContextTree (node t) (f.context $ t)


parent = move init
setContext c = move (\_ -> c)
root = move (\_ -> [])

isRoot (ContextTree n c) = null c


-- adds a new node to tree at given context, and points to it in the new context
addChild :: a -> ContextTree a -> ContextTree a
addChild cont t = let curcontext = context t
                      pos = head curcontext
                      nodes = children (node t) in
                  if null curcontext
                  then let newNodes = nodes ++ [Node cont []]
                       in ContextTree (Node (content (node t)) newNodes) [length newNodes - 1]
                  else let branch = addChild cont (ContextTree (nodes !! pos) (tail curcontext)) in
                       ContextTree (Node (content (node t))
                             (take pos nodes
                              ++ [node branch]
                              ++ drop (pos+1) nodes)) (pos : context branch)

-- applies f to the current context (considered as a tree) and returns modified t
modify :: (ContextTree a -> ContextTree a) -> ContextTree a -> ContextTree a
modify f t = let ctxt = context t
                 pos = head ctxt
                 root = node t -- current location
                 nodes = children root in
             if null ctxt -- this is the node to be modified
             then f t  -- modify it
             else let branch = modify f (ContextTree (nodes !! pos) (tail ctxt)) in -- otherwise recurse
                  ContextTree (Node (content root) 
                        (take pos nodes
                         ++ [node branch] -- insert the modified subtree in the right place
                         ++ drop (pos+1) nodes)) (pos : context branch)

addChild' cont t = modify change t where
    change (ContextTree node cnt) = ContextTree (Node (content node) newNodes) [length newNodes -1]
                           where newNodes = children node ++ [Node cont []]

modifyContextTreeCnt f = modify g where
    g (ContextTree node cnt) = ContextTree (Node (f (content node)) (children node)) cnt

getContent t = fetch (node t) (context t) where
    fetch n ctxt = case ctxt of
                     [] -> content n
                     (i:rest) -> fetch (children n !! i) rest

instance (Show a) => Show (Node a) where
    show node = 
        disp 0 node where
            disp :: (Show a) => Integer -> Node a -> String
            disp ind n = map (\_ -> ' ') [1..2*ind] ++ Prelude.show (content n)
                         ++ "\n" ++ concatMap (disp (ind+1)) (children n)

instance (Show a) => Show (ContextTree a) where
    show t = show (node t) ++ show (context t)

