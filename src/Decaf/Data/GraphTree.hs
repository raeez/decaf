module Decaf.Data.GraphTree where
import Data.Graph.Inductive
import Decaf.Data.Tree

type GraphTree = ([GraphNode], [GraphEdge])
type GraphNode = (Int, String)
type GraphEdge = (Int, Int, String)

-- | Construct a GraphViz graph from a 'Decaf.Data.Tree'
buildGraph :: Tree String -> Gr String String
buildGraph program = mkGraph nodes edges :: Gr String String
       where
        (nodes, edges) = buildGraphTree program

buildGraphTree :: Tree String -> GraphTree
buildGraphTree = extract . numberTree 0

-- |we then number the tree Tree String -> Tree Int String
-- finally, we convert our NumberTree into a DotGraph
--
extract :: (Int, [GraphNode], [GraphEdge]) -> GraphTree
extract (_, nodes, edges) = (nodes, edges)

-- | Recursively walk a 'Decaf.Data.Tree' in-order to
-- a) number the nodes with integral values in order visited
-- b) convert the 'Decaf.Data.Tree' into a tuple of ('GraphNode', 'GraphEdge')
-- input 'new' corresponds to the starting integer for the numbering scheme
-- explicitly passes state as:
--    (Int, _, _)         -> corresponds to the last utilized node label
--    (_, [GraphNode], _) -> corresponds to the list of labeled nodes contained in the subtree rooted at this node (root inclusive)
--    (_, _, [GraphEdge]) -> corresponds to the list of edges contained in the subtree rooted at this node (root inclusive)
numberTree :: Int -> Tree String -> (Int, [GraphNode], [GraphEdge])
numberTree new Nil = (new-1, [], []) -- (new-1) because the convention is to return the 'last labeled number'; i.e. we did not utilize label 'new', hence decrement the label counter
numberTree new (Node val children) = (n', nodes, edges)
                  where
                    nodes = (new, val) : nodes' -- label this node 'new'
                    (n', nodes', edges) = -- label this node's children
                            case children of
                                Nothing -> (new, [], []) -- new is still the last utilized label
                                Just a -> let (n'''', nodes'''', edges'''', ch) = numberChildren (new+1) a -- number the children starting from ('new'+1)
                                          in (n'''', nodes'''', edges'''' ++ map buildEdge ch)
                                          where buildEdge cid = (new, cid, "")

-- | Recursively walk down a list of a Node's children in a 'Decaf.Data.Tree'
-- returns passes state as:
--    (Int, _, _, _)         -> the last utilized node label
--    (_, [GraphNode], _)    -> the list of nodes contained in this children list
--    (_, _, [GraphEdge], _) -> the list of edges contained in this children list
--    (_, _, _, [Int])       -> the list of labels corresponding to the labels of the children in this list; utilized by the caller of 'numberChildren' to create edges between the root of this children list, and all the nodes contained in the list
numberChildren :: Int -> [Tree String] -> (Int, [GraphNode], [GraphEdge], [Int])
numberChildren new [] = (new-1, [], [], []) -- (new-1) because the convention is to return the 'last labeled number'; i.e. we did not utilize the lable 'new', hence decrement the label counter
numberChildren new (x:xs) = let (n'', nodes'', edges'') = numberTree new x
                                (n''', nodes''', edges''', ch') = numberChildren (n''+1) xs
                            in (n''', nodes'' ++ nodes''', edges'' ++ edges''', [new | n'' >= new] ++ ch')
