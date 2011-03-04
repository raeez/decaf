module Decaf.Data.SymbolTable where
import Decaf.AST
import Decaf.Data.ContextTree

-- | A program's set of 'SymbolTable' is stored in a tree structure
type SymbolTree = ContextTree SymbolTable

-- | Program symbols are stored in the 'SymbolTable' structure
data SymbolTable = SymbolTable {
  symbolRecords :: [SymbolRecord],
  blockType :: BlockType
  } deriving (Show, Eq)

-- | Individual program symbol entries are stored in the 'SymbolRecord' structure
data SymbolRecord = VarRec DecafVar 
                  | MethodRec DecafMethod
                  | ArrayRec DecafArr
                  deriving (Show,Eq)

-- | Symbol representing the various styleof block in Decaf
data BlockType = ForBlock
               | IfBlock
               | MethodBlock DecafType
               | GlobalBlock
               | TrivialBlock
               deriving (Show,Eq)

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
