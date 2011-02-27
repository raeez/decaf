module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Pos
import System.Exit
import Monad

data Statement = Ass {targetID :: Identifier, sourceID :: Identifier}
               | Dec {newID :: Identifier}
               | Expr {exprID :: Identifier}
               | Block {contents :: [Statement]}
                 deriving Show

data SymbolRecord = VarRec {iden :: Identifier }
                    deriving Show 
--                  | MethodRec Identifier [VarRec]  -- name, parameter list


newtype SymbolTable = SymbolTable {symbolRecords :: [SymbolRecord]}
    deriving Show
type SymbolTree = Tree SymbolTable

newtype Checker a = Checker {runChecker :: (String, SymbolTree) -> (a, (String, SymbolTree))}

instance Monad Checker where

    return a = Checker (\s -> (a, s))
    m >>= f = Checker (\s -> let (a,(e,t)) = runChecker m s in runChecker (f a) (e,t))

pushError str = Checker (\(e,t) -> ((),(e++str++"\n",t)))
addSymbol id = Checker (\(e,t) -> ((), (e, modifyTreeCnt g t))) 
    where g st = SymbolTable ((VarRec id) : (symbolRecords st))
local m = Checker (\(e,t)-> let (a,(e',t')) = runChecker m (e, addChild (SymbolTable []) t) in
                            (a, (e', setContext (context t) t')))
getST  = Checker (\(e,t) -> (t,(e,t)))

lookNear id = do st <- getST
                 let recs = symbolRecords.getContent $ st
                 return (id `elem` (map iden recs))

lookFar id = do st <- getST
                return $ exists st
    where exists st = let recs = symbolRecords.getContent $ st in
                      id `elem` (map iden recs) ||
                         if isRoot st
                         then False
                         else exists $ parent st



type Identifier = String

checkStatement stmt = 
    case stmt of
      Ass t s -> do {
                   x <- lookFar t;
                   y <- lookFar s;
                   if x
                   then if y
                        then pushError ("Assignment: "++t++"="++s) >> return True
                        else pushError ("Undefined variable "++s) >> return False
                   else pushError ("Undefined variable "++t) >> return False
                 }
      Dec id -> lookNear id >>= \b ->
                if b 
                then pushError ("Repeat Declaration "++id) >> return False
                else pushError ("Declaration: " ++ id) >> addSymbol id >> return True
      Expr id -> lookFar id >>= \b ->
                 if b 
                 then pushError ("Expression: "++id) >> return True 
                 else pushError ("Undefined expression "++id) >> return False
      block@(Block _) -> do pushError "Block"
                            local (foldl (>>) (return True) (map checkStatement (contents block)))
                            return True -- change this


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
                case (runChecker (checkStatement val) ("", mkTree $SymbolTable [])) of
                  (_,(e,t)) -> putStrLn $ e ++ (show t)
            Left err -> putStrLn.show $ err




         