module Decaf.Data.SymbolTable where
import Decaf.IR.AST
import Decaf.Data.Zipper
import Decaf.Data.Tree

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
-- TODO 
data SymbolRecord = VarRec DecafVar SymbolicRegister
                  | MethodRec DecafMethod MethodLabel
                  | ArrayRec DecafArr GlobalOffset
                  deriving (Show, Eq)



-- | Symbol representing the various styleof block in Decaf
data BlockType = ForBlock
               | IfBlock
               | MethodBlock DecafType
               | GlobalBlock
               | TrivialBlock
               deriving (Show, Eq)

-- | Retrieve the a symbol's identifier; utilized in checking declarations
symID :: SymbolRecord -> DecafIdentifier
symID (VarRec v _) = varID v
symID (MethodRec m _) = methodID m
symID (ArrayRec a _) = arrayID a

-- | Retrieve a symbol's type; utilized in type checking
symType :: SymbolRecord -> DecafType
symType (VarRec v _) = varType v
symType (MethodRec m _) = methodType m
symType (ArrayRec a _) = arrayType a

-- | Create a new SymbolTree
mkSymbolTree :: SymbolTree
mkSymbolTree = mkZipper (SymbolTable [] GlobalBlock)




-- | A Symbolic Address assigned to the program symbol by the semantic checker;
-- resolved at register allocater (in the code generator)
type MethodLabel = (String, Int)
type SymbolicRegister = Int
type GlobalOffset = Int
type StringLabel = (String, Int)


data LabelCounter = LC
    { regCount :: Int
    , globalCount :: Int
    , methodCount :: Int
    , stringCount :: Int
    }

mkCounter = LC 0 0 0 0


data CounterState 
    = CS { csCounter :: LabelCounter
         }

data RegisterCounter a = RC {runRegisterCounter :: CounterState -> (a, CounterState)}

instance Monad RegisterCounter where
    return x = RC(\s -> (x,s))
    m >>= f = RC(\s -> let (x,s') = runRegisterCounter m s
                       in runRegisterCounter (f x) s')


getRegCount = RC (\s@(CS{csCounter=c}) -> 
                      let n = regCount c
                      in (n, s{csCounter=c{regCount = n+1}}))
getGlobalCount = RC (\s@(CS{csCounter=c}) -> 
                         let n = globalCount c
                         in (n, s{csCounter=c{regCount = n+1}}))
getMethodCount = RC (\s@(CS{csCounter=c}) -> 
                         let n = methodCount c
                         in (n, s{csCounter=c{regCount = n+1}}))
getStringCount = RC (\s@(CS{csCounter=c}) -> 
                         let n = stringCount c
                         in (n, s{csCounter=c{regCount = n+1}}))


numberTree :: Tree SymbolTable -> RegisterCounter (Tree SymbolTable)
numberTree t = 
    let tab = content t
    in
      do cont <- sequence (map numberRec (symbolRecords tab))
         cs <- sequence (map numberTree (children t))
         return $ Node tab{symbolRecords = cont} cs



numberRec :: SymbolRecord -> RegisterCounter SymbolRecord
numberRec v@(VarRec a b) = do c <- getRegCount
                              return $ VarRec a c
numberRec v@(MethodRec a b) = do c <- getMethodCount
                                 return $ MethodRec a ("meth", c)
numberRec v@(ArrayRec a b) = do c <- getGlobalCount
                                return $ ArrayRec a c
                
