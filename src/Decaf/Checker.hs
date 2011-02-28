module Decaf.Checker where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Pos
import System.Exit
import Monad
import Decaf.AST

data SymbolRecord = VarRec {symbol :: DecafVar }
                  | MethodRec {symbol :: DecafMethod}
                  | ArrayRec {symbol :: DecafArray}
                    deriving (Show,Eq)


-- access just the ID, for declaration checking
symID (VarRec v) = varID v
symID (MethodRec m) = methodID m
symID (ArrayRec a) = arrayID a

-- access just the type for type checking
symType (VarRec v) = varType v
symType (MethodRec m) = methodType m
symType (ArrayRec a) = arrayType a


newtype SymbolTable = SymbolTable {symbolRecords :: [SymbolRecord]
                                  , blockType :: BlockType}
    deriving Show, Eq

data BlockType = ForBlock | IfBlock | MethodBlock DecafType | GlobalBlock | TrivialBlock
                 deriving (Show,Eq)

type SymbolTree = Tree SymbolTable


newtype Checker a = Checker {runChecker :: (String, SymbolTree) -> (a, (String, SymbolTree))}


instance Monad Checker where

    return a = Checker (\s -> (a, s))
    m >>= f = Checker (\s -> let (a,(e,t)) = runChecker m s in runChecker (f a) (e,t))

pushError :: String -> Checker ()
pushError str = (Checker (\(e,t) -> ((),(e++str++"\n",t)))) >> (return False)

addSymbol :: SymbolRecord -> Checker ()
addSymbol sr = Checker (\(e,t) -> ((), (e, modifyTreeCnt g t))) 
    where g (SymbolTable rs bt) = SymbolTable (sr : rs) bt

local :: BlockType -> Checker a -> Checker a
local tp m = Checker (\(e,t)-> let (a,(e',t')) = runChecker m (e, addChild (SymbolTable [] tp) t) in
                            (a, (e', setContext (context t) t')))
getST :: Checker SymbolTree
getST  = Checker (\(e,t) -> (t,(e,t)))

lookNear :: DecafIdentifier -> Checker SymbolRecord
lookNear id = do st <- getST
                 let recs = symbolRecords.getContent $ st
                 return (find id (zip (map symID recs) recs))

lookFar :: DecafIdentifier -> Checker SymbolRecord
lookFar id = do st <- getST
                return $ exists st
    where exists st = let recs = symbolRecords.getContent $ st in
                      case (find id (zip (map symID recs) recs)) of
                        Nothing -> if isRoot st
                                   then Nothing
                                   else (exists $ parent st)
                        other -> other

inFor = do st <- getST
           return infor st

    where infor st =
              if (blockType.getContent.node $ st) == ForBlock
              then True
              else if isRoot st
                   then False
                   else inFor $ parent st)

inMethod :: Checker DecafType
inMethod = do st <- getST
              return $ inmethod st
    where inmethod st = 
              case blockType.getContent.node $ st of
                MethodBlock t -> Just t
                other -> if isRoot st
                         then Nothing
                         else inmethod $ parent st

type Identifier = String

{- to write : 

  getLocType -- location type
  checkExpr  -- returns Maybe DecafType
 

error pushing code needs line numbers
  

-}
checkStm :: DecafStm -> Checker Bool
checkStm stmt = 
    case stmt of 
      DecafAssignStm loc op expr -> 
          do lhs <- lookFar (decafID loc)
             t2 <- checkExpr expr
             let t1 = symID lhs
             case op of
               DecafEq -> if (t1 == t2)
                          then return True
                          else pushError ("Mismatched types in assignment statement") 
                                   >> return False
               other -> if (t1 == DInteger && t2 == DInteger)
                        then return True
                        else pushError ("Mismatched types in arithmetic assignment statement") 
                                 >> return False
      DecafMethodStm (DecafPureMethodCall dID args) -> 
          do ex <- lookFar dID
             case ex of 
               Just (MethodRec decafMethod) 
                   -> do types <- bindCat [] (map checkExpr args)
                         if all (== True) $ zipWith (==) types (map varType (methodArg decafMethod))
                         then return True
                         else pushError ("Incorrect argument type")
               Nothing -> pushError ("Undeclared method: " ++ dId)
          where 
            bindCat :: [DecafType] -> [Checker DecafType] -> Checker [DecafType]
            bindCat res args = 
                case args of
                  [] -> return res
                        other -> do t <- (head args)
                                    bindCat (res++t) (tail args)

      DecafIfStm expr block melse ->
          do case checkExpr expr of
               Just dt -> if dt == DBool
                          then return ()
                          else pushError ("If conditional must have type Boolean")
               Nothing -> return ()

             case melse of
               Just e -> checkBlock block IfBlock >> checkBlock e IfBlock
               Nothing -> checkBlock block IfBlock

      DecafForStm id expr1 expr2 block -> 
          do addSymbol $ VarRec $ DecafVar DInteger id
             t1 <- checkExpr expr1
             t2 <- checkExpr expr2
             if (t1 /= DInteger)
             then pushError ("Start expression in for loop must have type Integer")
             else return ()
             if (t2 /= DInteger)
             then pushError ("End expression in for loop must have type Integer")
             else return ()
             checkBlock block ForBlock

      DecafRetStm rv -> 
          do t <- inMethod -- returns method type, if there is one
                  case t of
                    None -> pushError ("return may only be used inside a method definition")
                    Just DVoid -> 
                        case rv of
                          None -> return True
                          other -> pushError ("Function declared void cannot return a value")
                    Just x ->
                        case rv of
                          None -> pushError ("Function declared to return a value must do so")
                          Just expr -> do rt <- checkExpr expr
                                          if rt == x
                                          then return True
                                          else pushError ("Function declared to return "++x++" cannot return "++rt)
                                                      

      DecafBreakStm -> guardFor
      DecafContStm  -> guardFor
          where guardFor = do b <- inFor
                              if b
                              then return True
                              else pushError ("break and continue may only be used inside for loops")
      BlockStm block -> checkBlock block TrivialBlock


checkBlock :: DecafBlock -> BlockType -> Checker Bool
checkBlock (DecafBlock vars stmts) btype 
    = do (foldl (>>) (return False) (map checkVarDec vars))
         (foldl (>>) (return False) (map checkStm stmts))



checkVarDec :: DecafVar -> Checker Bool
checkVarDec (DecafVar t id) = 
    do rec <- lookNear id
       case rec of 
         Nothing -> addSymbol $ VarRec $ DecafVar t id
         other -> pushError ("Variable "++id++" already defined at line number ")


checkFieldDec :: DecafField -> Checker Bool
checkFieldDec (DecafVarField var) = checkVarDec var
checkFieldDec (DecavArrField (DecafArray t id len)) = 
    do if len < 0 -- needs to be fixed


                          
          

      


---- TEMPORARY PARSING FUNCTION -----

parseID :: Parser String
parseID = many1 (noneOf ";=!\n {}")

parseStmt = (char '!' >> parseID >>= \s -> (char ';' >> (return $ Dec s)))
            <|> (try (do t <- parseID
                         char '='
                         s <- parseID
                         char ';'
                         return $ Ass t s))
            <|> (parseID >>= \s -> char ';' >> (return $ Expr s))
            <|> (do char '{'
                    stmts <- many parseStmt
                    char '}'
                    return $ Block stmts)






{- failed attempt at double linked trees


data Node a = Node (Node a) a [Node a]
            | Root a [Node a]

parent (Node x y z) = x
parent x@(Root _ _) = x

content (Node x y z) = y
content (Root y z)  = y

children (Node x y z) = z
children (Root y z) = z


instance (Show a) => Show (Node a) where
--show node@(Node x y z) = 
show :: (Show a) => Node a -> String
show node = 
    init $ disp 0 node where
        disp :: (Show a) => Integer -> Node a -> String
        disp ind n = (map (\_ -> ' ') [1..2*ind]) ++ Prelude.show (content n)
                     ++ "\n" ++ concatMap (disp (ind+1)) (children n)


appendChild a b = Node (parent a) (content a) (children a ++ [b])
pushChild parent content
    =  x where x = Node (appendChild parent x) content []

-}

data Node a = Node {content :: a, children :: [Node a]}
data Tree a = Tree {node :: Node a, context :: [Int]}

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
                 root = node t
                 nodes = children root in
             if null ctxt
             then f t
             else let branch = modify f (Tree (nodes !! pos) (tail ctxt)) in
                  Tree (Node (content root) 
                        ((take pos nodes) 
                         ++ [node branch] 
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

{-
main = do let a = mkTree "root"
          let a' = addChild' "child A" a
              a'' = addChild' "child A'" a'
          let a' = root a''
              a''' = addChild' "child B" a'
          putStrLn.show $ (parent a''')
          return ()

-}

main = do args <- getArgs 
          str <- readFile "chinput.txt"
          case parse parseStmt "" str of
            Right val -> 
                case (runChecker (checkStm val) ("", mkTree $SymbolTable [])) of
                  (_,(e,t)) -> putStrLn $ e ++ (show t)
            Left err -> putStrLn.show $ err




         