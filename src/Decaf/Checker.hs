module Decaf.Checker where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Pos
import System.Exit
import Monad
import Data.List
import Numeric
import Decaf.AST hiding (Node, Tree)
import Decaf.Parser
import Decaf.Scanner
import Decaf.Tokens


data SymbolRecord = VarRec DecafVar 
                  | MethodRec DecafMethod
                  | ArrayRec DecafArr
                    deriving (Show,Eq)


-- access just the ID, for declaration checking
symID (VarRec v) = varID v
symID (MethodRec m) = methodID m
symID (ArrayRec a) = arrayID a

-- access just the type for type checking
symType (VarRec v) = varType v
symType (MethodRec m) = methodType m
symType (ArrayRec a) = arrayType a


{- Symbol table type -}
data SymbolTable = SymbolTable {symbolRecords :: [SymbolRecord]
                               , blockType :: BlockType}
    deriving (Show, Eq)

data BlockType = ForBlock | IfBlock | MethodBlock DecafType | GlobalBlock | TrivialBlock
                 deriving (Show,Eq)

-- See tree implementation at bottom
type SymbolTree = Tree SymbolTable


{- Semantic checking monad -}
data Checker a = Checker {runChecker :: (String, SymbolTree) -> (a, (String, SymbolTree))}

instance Monad Checker where

    return a = Checker (\s -> (a, s))
    m >>= f = Checker (\s -> let (a,(e,t)) = runChecker m s in runChecker (f a) (e,t))


-- Monad manipulation functions
pushError :: String -> Checker Bool
pushError str = Checker (\(e,t) -> (False,(e++str++"\n",t)))

addSymbol :: SymbolRecord -> Checker Bool
addSymbol sr = Checker (\(e,t) -> (True, (e, modifyTreeCnt g t))) 
    where g (SymbolTable rs bt) = SymbolTable (sr : rs) bt

local :: BlockType -> Checker a -> Checker a
local tp m = Checker (\(e,t)-> let (a,(e',t')) = runChecker m (e, addChild (SymbolTable [] tp) t) in
                            (a, (e', setContext (context t) t')))
getST :: Checker SymbolTree
getST  = Checker (\(e,t) -> (t,(e,t)))

get :: Checker (String,SymbolTree)
get = Checker (\(e,t) -> ((e,t),(e,t)))

setTree :: SymbolTree -> Checker ()
setTree t = Checker(\(e,t') -> ((), (e,t)))

setCheckerContext :: TreeContext -> Checker()
setCheckerContext c = Checker(\(e,t)-> ((),(e,setContext c t)))


-- symbol table access functions
lookNear :: DecafIdentifier -> Checker (Maybe SymbolRecord)
lookNear id = do st <- getST
                 let recs = symbolRecords.getContent $ st
                 return (lookup id (zip (map symID recs) recs))

lookFar :: DecafIdentifier -> Checker (Maybe SymbolRecord)
lookFar id = do st <- getST
                return $ exists st
    where exists st = let recs = symbolRecords.getContent $ st in
                      case (lookup id (zip (map symID recs) recs)) of
                        Nothing -> if isRoot st
                                   then Nothing
                                   else (exists $ parent st)
                        other -> other

inFor :: Checker Bool
inFor = do st <- getST
           return $ infor st

    where infor st =
              if (blockType.getContent $ st) == ForBlock
              then True
              else if isRoot st
                   then False
                   else infor $ parent st

inMethod :: Checker (Maybe DecafType)
inMethod = do st <- getST
              return $ inmethod st
    where inmethod st = 
              case blockType.getContent $ st of
                MethodBlock t -> Just t
                other -> if isRoot st
                         then Nothing
                         else inmethod $ parent st

addRet :: Checker Bool
addRet = do st <- getST
            goFun st
            setCheckerContext (context st) -- goFun modifies context
            return True
    where goFun st = 
              case blockType.getContent $ st of
                MethodBlock _ -> 
                    do setCheckerContext (context st)
                       addSymbol $ VarRec $ DecafVar DecafVoid "return"
                other -> goFun $ parent st

{- to write : 

  error pushing code needs line numbers (pass line as argument to pushError)


  make sure -intlit's are scanned correctly
-}

{- Statement-like Semantic checkers, bottom-up order -}
checkStm :: DecafStm -> Checker Bool
checkStm stmt =                  
    case stmt of 
      DecafAssignStm loc op expr -> 
          do lhs <- lookFar (ident loc)
             case lhs of 
               Nothing -> pushError ("Undeclared variable "++(ident loc))
               Just lhs -> 
                   do case loc of
                        DecafArrLoc arrid ind ->
                            do indT <- checkExpr ind
                               if indT == DecafInteger
                                then return True
                                else pushError "Array index must be an integer"
                        _ -> return True

                      t2 <- checkExpr expr
                      let t1 = symType lhs
                      case op of
                        DecafEq -> if (t1 == t2)
                                   then return True
                                   else pushError ("Mismatched types in assignment statement") 
                                            >> return False
                        other -> if (t1 == DecafInteger && t2 == DecafInteger)
                                 then return True
                                 else pushError ("Mismatched types in arithmetic assignment statement") 
      DecafMethodStm m -> -- (DecafPureMethodCall dID args) -> 
              checkMethodArgs m >> return False
{-          do ex <- lookFar dID
             case ex of 
               Just (MethodRec decafMethod) -> 
                   do types <- bindCat [] (map checkExpr args)
                      if (all (== True) $ zipWith (==) types (map varType (methodArg decafMethod)))
                        then return True
                        else pushError ("Incorrect argument type")
               Nothing -> pushError ("Undeclared method: " ++ dID)
          where 
            bindCat :: [DecafType] -> [Checker DecafType] -> Checker [DecafType]
            bindCat res args = 
                case args of
                  [] -> return res
                  other -> do t <- (head args)
                              bindCat (res++[t]) (tail args) -}

      DecafMethodStm (DecafMethodCallout id args) ->
          foldl (>>) (return False) (map checkCalloutArg args) >> return True

      DecafIfStm expr block melse ->
          do dt <- checkExpr expr
             if dt == DecafBoolean
              then return True
              else pushError ("If conditional must have type Boolean")

             case melse of
               Just e -> checkBlock block IfBlock >> checkBlock e IfBlock
               Nothing -> checkBlock block IfBlock

      DecafForStm id expr1 expr2 block -> 
          do addSymbol $ VarRec $ DecafVar DecafInteger id
             t1 <- checkExpr expr1
             t2 <- checkExpr expr2
             if (t1 /= DecafInteger)
               then pushError ("Start expression in for loop must have type Integer")
               else return True
             if (t2 /= DecafInteger)
               then pushError ("End expression in for loop must have type Integer")
               else return True
             checkBlock block ForBlock

      DecafRetStm rv ->  -- ADD CODE FOR addSymbol ret
          do t <- inMethod -- returns method type, if there is one
             case t of
               Nothing -> pushError ("return may only be used inside a method definition")
               Just DecafVoid -> 
                   case rv of
                     Nothing -> return True
                     other -> pushError ("Function declared void cannot return a value")
               Just x ->
                   case rv of
                     Nothing -> pushError ("Function declared to return a value cannot return void")
                     Just expr -> do rt <- checkExpr expr
                                     if rt == x
                                       then return True
                                       else pushError ("Function declared to return "++(show x)++" cannot return "++(show rt))
             case t of
               Nothing -> return False
               other -> addRet -- hack, so that checkMethod knows whether a return statement occurred in the body; no legal var can have ID="return"
                                                      

      DecafBreakStm -> guardFor
      DecafContStm  -> guardFor
      DecafBlockStm block -> checkBlock block TrivialBlock

    where guardFor = do b <- inFor
                        if b
                         then return True
                         else pushError ("break and continue may only be used inside for loops")


checkBlock :: DecafBlock -> BlockType -> Checker Bool
checkBlock (DecafBlock vars stmts) btype 
    = local btype (do (foldl (>>) (return False) (map checkVarDec vars))
                      (foldl (>>) (return False) (map checkStm stmts))
                      case btype of
                        MethodBlock t ->
                            do mrec <- lookNear "return"
                               case mrec of
                                 Nothing -> if t == DecafVoid
                                            then return True
                                            else pushError "Missing return statement" -- line number of block
                                 other -> return True
                        other -> return True)

checkCalloutArg :: DecafCalloutArg -> Checker Bool
checkCalloutArg carg =
    case carg of
      DecafCalloutArgExpr expr -> checkExpr expr >> return True
      DecafCalloutArgStr _ -> return True

checkVarDec :: DecafVar -> Checker Bool
checkVarDec (DecafVar t id) = 
    do rec <- lookNear id
       case rec of 
         Nothing -> addSymbol $ VarRec $ DecafVar t id
         other -> pushError ("Variable "++id++" already defined at line number ")


checkFieldDec :: DecafField -> Checker Bool
checkFieldDec (DecafVarField var) = checkVarDec var
checkFieldDec (DecafArrField arr@(DecafArr t id len)) = 
    do rec <- lookNear id
       case rec of
         Nothing -> do checkExpr (DecafLitExpr (DecafIntLit len))
                       let l = readDInt len
                       if l <= 0 -- needs to be fixed once I make checkLiteral
                        then pushError ("Arrays must have positive length")
                        else addSymbol $ ArrayRec $ arr
         other -> pushError ("Array "++id++" already defined at line number ")

checkMethodDec :: DecafMethod -> Checker Bool
checkMethodDec meth@(DecafMethod t id args body@(DecafBlock bvars bstms)) = 
    do rec <- lookNear id
       case rec of
         Nothing -> do foldl (>>) (return False) (map (addSymbol.VarRec) args)
                       addSymbol $ MethodRec meth
                       checkBlock (DecafBlock (bvars) bstms) (MethodBlock t) 


         other -> pushError ("Function "++id++" already defined at line number ")


checkProgram :: DecafProgram -> Checker Bool
checkProgram (DecafProgram fields methods) = 
    do foldl (>>) (return False) (map checkFieldDec fields)
       foldl (>>) (return False) (map checkMethodDec methods)
       b <- lookNear "main"
       case b of
         Just (MethodRec (DecafMethod t id args body)) -> 
            if null args
            then return True
            else pushError "Method \"main\" must have empty parameter list"
         other -> pushError "Must define method main"

                          
{- Type checking -}

checkExpr :: DecafExpr -> Checker DecafType
checkExpr expr = 

    let lookupID id = 
            do mrec <- lookFar id
               case mrec of
                 Nothing -> pushError ("Undefined variable "++id) >> return DecafVoid
                 Just rec -> return $ symType rec
    in
      case expr of
        DecafLocExpr (DecafVarLoc id) ->
            do mrec <- lookFar id
               case mrec of
                 Nothing -> pushError ("Undefined variable "++id) >> return DecafVoid
                 Just rec -> 
                     case rec of
                       ArrayRec _ -> pushError ("Must specify array index") >> return DecafVoid
                       other -> return $ symType rec

        DecafLocExpr (DecafArrLoc id ind) ->
            do arrT <- lookupID id 
               indT <- checkExpr ind
               if indT == DecafInteger
                then return True
                else pushError "Array index must be an integer"
               return arrT


        DecafMethodExpr m -> --(DecafPureMethodCall dID args) -> 
            checkMethodArgs m
{-            do ex <- lookFar dID
               case ex of 
                 Just (MethodRec decafMethod) -> 
                   do types <- bindCat [] (map checkExpr args)
                      if (all (== True) $ zipWith (==) types (map varType (methodArg decafMethod)))
                        then return False
                        else pushError ("Incorrect argument type")
                      return (methodType decafMethod)
                 Nothing -> pushError ("Undeclared method: " ++ dID) >> return DecafVoid
          where 
            bindCat :: [DecafType] -> [Checker DecafType] -> Checker [DecafType]
            bindCat res args = 
                case args of
                  [] -> return res
                  other -> do t <- (head args)
                              bindCat (res++[t]) (tail args) -}

        DecafMethodExpr (DecafMethodCallout id args) ->
            do foldl (>>) (return False) (map checkCalloutArg args)
               return DecafInteger

        DecafLitExpr lit ->
            case lit of
              DecafIntLit int -> 
                  checkInt int
              DecafBoolLit _ -> return DecafBoolean
              other -> pushError "THIS SHOULDN'T HAPPEN" >> return DecafVoid

        DecafBinExpr expr1 op expr2 ->
            do t1 <- checkExpr expr1
               t2 <- checkExpr expr2
               case op of 
                 DecafBinArithOp _ -> 
                     do
                       if t1 /= DecafInteger
                        then pushError "Argument of arithmetic operator must be integral"
                        else return True -- doesn't matter
                       if t2 /= DecafInteger
                        then pushError "Argument of arithmetic operator must be integral"
                        else return True -- doesn't matter
                       return DecafInteger
                 DecafBinRelOp _ ->
                     do
                       if t1 /= DecafInteger
                        then pushError "Argument of relative operator must be integral"
                        else return True
                       if t2 /= DecafInteger
                        then pushError "Argument of relative operator must be integral"
                        else return True
                       return DecafBoolean
                 DecafBinEqOp _ -> 
                       do if t1 /= t2
                           then pushError "Arguments of \"==\" must be of the same type"
                           else return True
                          return DecafBoolean
                 DecafBinCondOp _ ->
                     do
                       if t1 /= DecafBoolean
                        then pushError "Argument of logical operator must be boolean"
                        else return True
                       if t2 /= DecafBoolean
                        then pushError "Argument of logical operator must be boolean"
                        else return True
                       return DecafBoolean              
        DecafNotExpr expr  ->
            do t <- checkExpr expr
               if t /= DecafBoolean
                then pushError "Argument of logical not must be boolean"
                else return True
               return DecafBoolean

        DecafMinExpr expr ->
            do t <- checkExpr expr
               if t /= DecafInteger
                then pushError "Can only negate an integral expression"
                else return True
               return DecafInteger
        DecafParenExpr expr -> checkExpr expr

    where checkInt int = 
              let val = readDInt int in
              if val > 2^31-1 || val < -2^31
              then pushError "Integer literal too large" >> return DecafInteger -- maybe should be void
              else return DecafInteger

checkMethodArgs :: DecafMethodCall -> Checker DecafType
checkMethodArgs (DecafPureMethodCall dID args) =

            do ex <- lookFar dID
               case ex of 
                 Just (MethodRec decafMethod) -> 
                   do types <- bindCat [] (map checkExpr args)
                      if (all (== True) $ zipWith (==) types (map varType (methodArg decafMethod)))
                                  && length types == length (methodArg decafMethod)
                        then return False
                        else pushError ("Incorrect argument type")
                      return (methodType decafMethod)
                 Nothing -> pushError ("Undeclared method: " ++ dID) >> return DecafVoid
          where 
            bindCat :: [DecafType] -> [Checker DecafType] -> Checker [DecafType]
            bindCat res args = 
                case args of
                  [] -> return res
                  other -> do t <- (head args)
                              bindCat (res++[t]) (tail args)

checkMethodArgs _ = return DecafInteger


readDInt :: DecafInteger -> Integer
readDInt int = 
    case int of 
      DecafDec s -> (read s) :: Integer
      DecafHex s -> fst.head.(Numeric.readHex) $ s

-- system call



{- Tree implementation -}

data Node a = Node {content :: a, children :: [Node a]}
data Tree a = Tree {node :: Node a, context :: TreeContext}

type TreeContext = [Int]

mkTree :: a -> Tree a
mkTree content = Tree (Node content []) []

move :: ([Int] -> [Int]) -> Tree a -> Tree a
move f t = Tree (node t) (f.context $ t)


parent = move init
setContext c = move (\_ -> c)
root = move (\_ -> [])

isRoot (Tree n c) = null c


-- adds a new node to tree at given context, and points to it in the new context
addChild :: a -> Tree a -> Tree a
addChild cont t = let curcontext = context t
                      pos = head curcontext
                      nodes = children (node t) in
                  if null curcontext
                  then let newNodes = nodes ++ [(Node cont [])] in
                       Tree (Node (content (node t)) newNodes) [length newNodes - 1]
                  else let branch = addChild cont (Tree (nodes !! pos) (tail curcontext)) in
                       Tree (Node (content (node t))
                             ((take pos nodes) 
                              ++ [node branch]
                              ++ drop (pos+1) nodes)) (pos : (context branch))

-- applies f to the current context (considered as a tree) and returns modified t
modify :: (Tree a -> Tree a) -> Tree a -> Tree a
modify f t = let ctxt = context t
                 pos = head ctxt
                 root = node t -- current location
                 nodes = children root in
             if null ctxt -- this is the node to be modified
             then f t  -- modify it
             else let branch = modify f (Tree (nodes !! pos) (tail ctxt)) in -- otherwise recurse
                  Tree (Node (content root) 
                        ((take pos nodes) 
                         ++ [node branch] -- insert the modified subtree in the right place
                         ++ drop (pos+1) nodes)) (pos : (context branch))

addChild' cont t = modify change t where
    change (Tree node cnt) = Tree (Node (content node) newNodes) [length newNodes -1]
                           where newNodes = children node ++ [(Node cont [])]

modifyTreeCnt f = modify g where
    g (Tree node cnt) = Tree (Node (f (content node)) (children node)) cnt

getContent t = fetch (node t) (context t) where
    fetch n ctxt = case ctxt of
                     [] -> content n
                     (i:rest) -> fetch ((children n) !! i) rest

                         

instance (Show a) => Show (Node a) where
    show node = 
        disp 0 node where
            disp :: (Show a) => Integer -> Node a -> String
            disp ind n = (map (\_ -> ' ') [1..2*ind]) ++ Prelude.show (content n)
                         ++ "\n" ++ concatMap (disp (ind+1)) (children n)

instance (Show a) => Show (Tree a) where
    show t = show (node t) ++ show (context t)



-- returns true if no errors, false otherwise
checker i =                 
    case ps program i of
      RSuccess prog -> 
          case (runChecker (checkProgram prog) ("", mkTree $SymbolTable [] GlobalBlock)) of
            (_,(e,t)) -> if length e > 0
                         then False
                         else True
---      RError str -> False