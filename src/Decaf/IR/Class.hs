module Decaf.IR.Class where
import Decaf.Data.Tree

-- |Abstract Syntax: Every node in the Abstract Syntax tree track's it's parsed source pos as a tuple of (line, column)
type DecafPosition = (Int, Int)

-- | IRNode encapsulates common operations on the Abstract Syntax Tree
-- @pos - retrieves the parsed source code position of the 'ASTNode' as a 'DecafPosition'
-- @pp - pretty print the 'ASTNode'
-- @treeify - convert the subtree rooted at the 'ASTNode' into a generic 'Tree'
class IRNode a where
    pos :: a -> DecafPosition   -- return the source position
    pp :: a -> String           -- pretty print
    treeify :: a -> Tree String -- turn into a generic tree
