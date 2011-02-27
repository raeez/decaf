module Main
where
import Data.Graph.Inductive
import Data.GraphViz
import Decaf.Parser
import Decaf.AST
import System.Process
import System.Environment
import System.Exit
import Data.List

main :: IO ()
main = do
        args <- getArgs
        case args of
          [inputFile] -> (readFile inputFile) >>= parse inputFile
          otherwise -> putStrLn "error, must specify the input file as the first and only arg"

parse inputFile input = do
          case ps program input of
              RSuccess a -> putStrLn (show a) >> outputGraph a >> convertToPs >> (putStrLn $ show $ convertProgram a) >> exitSuccess
              RError e -> putStrLn ("Error:\n" ++ (show e)) >> exitFailure
          where
            outputFile = inputFile ++ ".out"
            graphText =  printDotGraph . (graphToDot graphParams) . buildGraph
            outputGraph a = (putStrLn $ graphText a) >> (writeFile outputFile $ graphText a)
            convertToPs = (runCommand $ "dot -Tpng " ++ outputFile ++ " -o" ++ outputFile ++ ".png") >> (return $ putStrLn "")

graphParams :: GraphvizParams String String () String
graphParams = nonClusteredParams {
    globalAttributes = ga,
    fmtNode = fn,
    fmtEdge = fe
  }
  where
    ga = [
      GraphAttrs [
        (BgColor . X11Color) Transparent
        ],
      NodeAttrs [
        (FillColor . X11Color) White,
        Style [SItem Filled []]
        ]
      ]

    fn (n,l) = [(Label . StrLabel) l]
    fe (f,t,l) = [(Label . StrLabel) l]

type Table a = [a]

 --numberTree :: Eq a => Tree a -> State (Table a) (Tree Int)
 --numberTree Nil = return Nil
 --numberTree (Node val@(i,v) t1 t2) =  do
                                        --nv <- numberNode val
                                        --nt1 <- numberTree t1
                                        --nt2 <- numberTree t2
                                        --return (Node nv nt1 nt2)
                                      --where
                                        --numberNode :: Eq a => a -> State (Table a) Int -- must return a tuple of (id, val)
                                        --numberNode val@(i, v) = do 
                                                                  --table <- get
                                                                  --(newTable, numberedNode) <- return (nNode val table)
                                                                  --put newTable
                                                                  --return numberedNode

                                        --nNode::  (Eq a) => a -> Table a -> (Table a, Int)
                                        --nNode val@(i, v) table = (table ++ [val], length table)

 --numTree :: (Eq a) => Tree a -> Tree Int
 --numTree t = evalState (numberTree t) []
 --testTree = Node "Zero" (Node "One" (Node "Two" Nil Nil) (Node "One" (Node "Zero" Nil Nil) Nil)) Nil
 --numTree testTree => Node 0 (Node 1 (Node 2 Nil Nil) (Node 1 (Node 0 Nil Nil) Nil)) Nil



buildGraph d = mkGraph nodes edges :: Gr String String
               where
                  (nodes, edges) = ([], [])
-- convert AST -> Tree a b
-- number tree
-- convert numebered tree to graph
--
data Tree a b = Node (a, b) (Maybe [Tree a b])
              | Nil
              deriving (Eq, Show)

pretty_var (DecafVar ty ident) = "Var("++ show ty ++ "|" ++ show ident ++ ")"
pretty_arr (DecafArr ty ident len) = "Arr(" ++ show ty ++ "|" ++ show ident ++"["++show len ++ "])"
pretty_meth (DecafMethod ty ident args body) = "Method(" ++ show ty ++ "|" ++ show ident ++ "|" ++ show args ++ ")"
pretty_op (a) = show a
pretty_arr_expr(DecafArrLoc ident expr) = "Arr(" ++ show ident ++ "[])"
convertProgram :: DecafProgram -> Tree Int String
convertProgram p  = Node (0, "Program") (Just children)
                    where
                      fs = map convertField (fields p)
                      ms = map convertMethod (methods p)
                      children = fs ++ ms

convertField :: DecafField -> Tree Int String
convertField (DecafVarField var@(DecafVar ty ident)) = convertVar var
convertField (DecafArrField arr@(DecafArr ty ident len)) = Node (0, pretty_arr arr) Nothing

convertVar :: DecafVar -> Tree Int String
convertVar var@(DecafVar ty ident) = Node (0, pretty_var var) Nothing

convertMethod :: DecafMethod -> Tree Int String
convertMethod meth@(DecafMethod ty ident args body) = Node(0, pretty_meth meth) (Just $ convertBlock body)

convertBlock :: DecafBlock -> [Tree Int String]
convertBlock (DecafBlock vars stms) = vs ++ ss
                                      where
                                        vs = map convertVar vars
                                        ss = map convertStm stms

convertStm :: DecafStm -> Tree Int String
convertStm (DecafAssignStm loc op expr) = Node (0, pretty_op op) (Just $ [convertLoc loc] ++ [convertExpr expr])
convertStm (DecafMethodStm (DecafPureMethodCall ident args)) = Node (0, "PureMethodcall") (Just $ [convertIdent ident] ++ (map convertExpr args))
convertStm (DecafMethodStm (DecafMethodCallout ident args)) = Node (0, "MethodCallOut") (Just $ [convertStr ident] ++ (map convertCalloutArg args))
convertStm (DecafIfStm expr block elseblock) = Node (0, "if") (Just $ [convertExpr expr] ++ convertBlock block ++ elseblock')
                                              where
                                                elseblock' = case convertElseBlock elseblock of
                                                                Just a -> a
                                                                Nothing -> []
convertStm (DecafForStm ident expr expr' block) = Node (0, "for") (Just $ [convertIdent ident] ++ [convertExpr expr] ++ [convertExpr expr'] ++ convertBlock block)
convertStm (DecafRetStm expr) = Node (0, "ret") $ convertRetExpr expr
convertStm (DecafBreakStm) = Node (0, "brk") Nothing
convertStm (DecafContStm) = Node (0, "cnt") Nothing
convertStm (DecafBlockStm block) = Node (0, "blockStm") (Just $ convertBlock block)

convertRetExpr :: (Maybe DecafExpr) -> (Maybe [Tree Int String])
convertRetExpr (Just expr) = Just $ [convertExpr expr]
convertRetExpr Nothing = Nothing

convertElseBlock :: (Maybe DecafBlock) -> (Maybe [Tree Int String])
convertElseBlock (Just block) = Just $ convertBlock block
convertElseBlock Nothing = Nothing

convertCalloutArg :: DecafCalloutArg -> Tree Int String
convertCalloutArg (DecafCalloutArgExpr expr) = convertExpr expr
convertCalloutArg (DecafCalloutArgStr str) = convertStr str

convertIdent :: DecafIdentifier -> Tree Int String
convertIdent (DecafIdentifier str) = Node(0, "IDENT("++ str ++")") Nothing

convertLoc :: DecafLoc -> Tree Int String
convertLoc var@(DecafVarLoc ident) = convertIdent ident
convertLoc arr@(DecafArrLoc ident expr) = Node (0, pretty_arr_expr arr) (Just $ [convertExpr expr])

convertExpr :: DecafExpr -> Tree Int String
convertExpr expr = Nil --fix

convertStr :: DecafString -> Tree Int String
convertStr (DecafString str) = Node (0, "STR("++str++")") Nothing
