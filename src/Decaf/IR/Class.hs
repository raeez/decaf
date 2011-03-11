module Decaf.IR.Class where
import Decaf.Data.Tree

-- | A 'DecafPosition' is a tuple of line and column number, corresponding
-- to a position in the original source code.
type DecafPosition = (Int, Int)

-- | IRNode encapsulates common operations on intermediate codes
class IRNode a where
    -- | The 'pos' function retrieves the parsed source code position
    -- of the 'ASTNode' as a 'DecafPosition'
    pos :: a -> DecafPosition

    -- | The 'pp' function formats the IR node (pretty-print)
    -- into a human-readable string.
    pp :: a -> String

    -- | The 'treeify' function converts the IRNode and all
    -- subtrees into a homogenous tree, where each node holds
    -- it's content cast into a string.
    treeify :: a -> Tree String -- turn into a generic tree
