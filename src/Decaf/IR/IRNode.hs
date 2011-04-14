module Decaf.IR.IRNode where
import Decaf.Data.Tree

-- | IRNode encapsulates common operations on intermediate codes
class IRNode a where
    -- | The 'pp' function formats the IR node (pretty-print)
    -- into a human-readable string.
    pp :: a -> String

    -- | The 'treeify' function converts the IRNode and all
    -- subtrees into a homogenous tree, where each node holds
    -- it's content cast into a string.
    treeify :: a -> Tree String -- turn into a generic tree

-- | A 'DecafPosition' is a tuple of line and column number, corresponding
-- to a position in the original source code.
type DecafPosition = (Int, Int)

class ConcretePosition a where
    -- | The 'pos' function returns the concrete position associated
    -- with an an abstract element.
    pos :: a -> DecafPosition
