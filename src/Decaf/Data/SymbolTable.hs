module Decaf.Data.SymbolTable where
import Decaf.AST
import Decaf.Data.Zipper

-- | A program's set of 'SymbolTable' is stored in a tree structure
type SymbolTree = Zipper SymbolTable

-- | A program's SymbolTable Context
type SymbolTreeContext = Context SymbolTable

-- | Program symbols are stored in the 'SymbolTable' structure
data SymbolTable = SymbolTable
    { symbolRecords :: [SymbolRecord]
    , blockType :: BlockType
    } deriving (Show, Eq)

-- | Individual program symbol entries are stored in the 'SymbolRecord' structure
data SymbolRecord = VarRec DecafVar SymbolicAddress
                  | MethodRec DecafMethod SymbolicAddress
                  | ArrayRec DecafArr SymbolicAddress
                  deriving (Show, Eq)

data SymbolicAddress = SymbolicRegister RegisterLabel
                     | GlobalOffset Offset
                     | FrameOffset Offset
                     deriving (Show, Eq)

type Offset = Int
type RegisterLabel = String

-- | Symbol representing the various styleof block in Decaf
data BlockType = ForBlock
               | IfBlock
               | MethodBlock DecafType
               | GlobalBlock
               | TrivialBlock
               deriving (Show, Eq)

-- | Retrieve the a symbol's identifier; utilized in checking declarations
symID :: SymbolRecord -> DecafIdentifier
symID (VarRec v) = varID v
symID (MethodRec m) = methodID m
symID (ArrayRec a) = arrayID a

-- | Retrieve a symbol's type; utilized in type checking
symType :: SymbolRecord -> DecafType
symType (VarRec v) = varType v
symType (MethodRec m) = methodType m
symType (ArrayRec a) = arrayType a

-- | Create a new SymbolTree
mkSymbolTree :: SymbolTable -> SymbolTree
mkSymbolTree = mkZipper
