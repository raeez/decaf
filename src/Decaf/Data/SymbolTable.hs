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
data SymbolRecord = VarRec DecafVar SymbolicRegister
                  | MethodRec DecafMethod MethodLabel
                  | ArrayRec DecafArr GlobalOffset
                  | StringRec DecafString StringLabel
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

-- | Lookup a symbol in the current SymbolTable
symLookup ::  String -> SymbolTable -> Maybe (Int, SymbolRecord)
symLookup ident table = ilookup 0 ident (zip (map symID recs) recs)
  where
    recs = symbolRecords table
    ilookup :: Int -> String -> [(String, SymbolRecord)] -> Maybe (Int, SymbolRecord)
    ilookup _ _ [] = Nothing
    ilookup i id ((key, val):xs) = if key == id
                                    then Just (i, val)
                                    else ilookup (i+1) id xs

-- | Lookup a symbol in the current SymbolTable, and all parent SymbolTable's
globalSymLookup :: String -> SymbolTree -> Maybe SymbolRecord
globalSymLookup ident st = let table = (content . tree) st
                           in case symLookup ident table of
                               Nothing -> if isRoot st
                                            then Nothing
                                            else globalSymLookup ident (parent st)
                               Just (_, a) -> Just a
-- | Create a new SymbolTree
mkSymbolTree :: SymbolTree
mkSymbolTree = mkZipper (SymbolTable [] GlobalBlock)

-- | A Symbolic Address assigned to the program symbol;
-- resolved in the global register allocator
type SymbolicRegister = Int

-- | A method label assigned to the program symbol;
-- resolved in the translator
type MethodLabel = (String, Int)

-- | A global offset assigned to the program symbol;
-- resolved in the final code generator
type GlobalOffset = Int

-- | A global string identifier,
-- resolved in the final code generator
type StringLabel = (String, Int)

data LabelCounter = LabelCounter
    { regCount :: Int
    , globalCount :: Int
    , methodCount :: Int
    , stringCount :: Int
    }

mkCounter :: LabelCounter
mkCounter = LabelCounter 0 0 0 0

data CounterState = CounterState
    { csCounter :: LabelCounter }

data RegisterCounter a = RegisterCounter
    { runRegisterCounter :: CounterState -> (a, CounterState) }

instance Monad RegisterCounter where
    return x = RegisterCounter(\s -> (x,s))
    m >>= f = RegisterCounter(\s ->
                  let (x, s') = runRegisterCounter m s
                  in runRegisterCounter (f x) s')

getRegCount = RegisterCounter (\s@(CounterState{csCounter=c}) ->
                      let n = regCount c
                      in (n, s{csCounter=c{regCount = n+1}}))

getGlobalCount = RegisterCounter (\s@(CounterState{csCounter=c}) ->
                         let n = globalCount c
                         in (n, s{csCounter=c{globalCount = n+1}}))

getMethodCount = RegisterCounter (\s@(CounterState{csCounter=c}) ->
                         let n = methodCount c
                         in (n, s{csCounter=c{methodCount = n+1}}))

getStringCount = RegisterCounter (\s@(CounterState{csCounter=c}) ->
                         let n = stringCount c
                         in (n, s{csCounter=c{stringCount = n+1}}))

numberTree :: Tree SymbolTable -> RegisterCounter (Tree SymbolTable)
numberTree t = 
    let table = content t
    in do cont <- sequence $ map numberRec (symbolRecords table)
          cs <- sequence $ map numberTree (children t)
          return $ Node table{symbolRecords = cont} cs

numberRec :: SymbolRecord -> RegisterCounter SymbolRecord
numberRec (VarRec a _) = do c <- getRegCount
                            return $ VarRec a c

numberRec (MethodRec a _) = do c <- getMethodCount
                               return $ MethodRec a ("meth", c)

numberRec (ArrayRec a _) = do c <- getGlobalCount
                              return $ ArrayRec a c
numberRec (StringRec a _) = do c <- getStringCount
                               return $ String a c
