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
import Control.Monad.State

main :: IO ()
main = do
        args <- getArgs
        case args of
          [inputFile] -> (readFile inputFile) >>= parse inputFile
          otherwise -> putStrLn "error, must specify the input file as the first and only arg"

parse inputFile input = do
          case ps program input of
              RSuccess a -> outputRaw a  >> outputGraph a >> convertToPs >> exitSuccess
              RError e -> putStrLn ("Error:\n" ++ (show e)) >> exitFailure
          where
            outputFile = inputFile ++ ".out"
            graphText =  printDotGraph . (graphToDot graphParams) . buildGraph
            outputGraph a = (putStrLn $ show $ (extract . numberTree 0 . convertProgram) a) >> (putStrLn $ graphText a) >> (writeFile outputFile $ graphText a) 
            convertToPs = (runCommand $ "dot -Tpng " ++ outputFile ++ " -o" ++ outputFile ++ ".png") >> (return $ putStrLn "")
            outputRaw = putStrLn . show

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

-- here we rewrite our AST -> Tree String
-- we then number the tree Tree String -> Tree Int String
-- finally, we convert our NumberTree into a DotGraph
--
data Tree a = Node a (Maybe [Tree a])
            | Nil
            deriving (Eq, Show)

numberTree n Nil = (n-1, [], [])
numberTree n (Node val children) = (n', nodes, edges)
                                    where
                                      nodes = [(n, val)] ++ nodes'
                                      (n', nodes', edges) = case children of
                                                                Nothing -> (n, [], [])
                                                                Just a -> let (n'''', nodes'''', edges'''', ch) = numberChildren (n+1) a in
                                                                          (n'''', nodes'''', edges'''' ++ (map buildEdge ch))
                                                            where buildEdge cid = (n, cid, "")

                                      numberChildren new (x:xs) = let (n'', nodes'', edges'') = numberTree (new) x
                                                                      (n''', nodes''', edges''', ch') = numberChildren (n''+1) xs in
                                                                  (n''', nodes'' ++ nodes''', edges'' ++ edges''', (if n'' >= new then [new] else []) ++ ch')
                                      numberChildren new [] = (new-1, [], [], [])

extract (a,b,c) = (b,c)
buildGraph d = mkGraph nodes edges :: Gr String String
               where
                  (nodes, edges) = (extract . numberTree 0 . convertProgram) d

pretty_type (DecafInteger) = "int"
pretty_type (DecafVoid) = "void"
pretty_type (DecafBoolean) = "bool"
pretty_int (DecafDec s) = "0d"++s
pretty_int (DecafHex h) = "0x"++h
pretty_var (DecafVar ty ident) = "VAR "++ pretty_type ty++ " " ++ ident
pretty_arr (DecafArr ty ident len) = "ARR " ++ pretty_type ty ++ " " ++ ident ++"["++ pretty_int len ++ "]"
pretty_meth (DecafMethod ty ident args body) = "METHOD " ++ pretty_type ty ++ " " ++  ident ++ " " ++ (concat $ intersperse " " $ map pretty_var args)
pretty_asop (DecafEq) = "<="
pretty_asop (DecafPlusEq) = "<+="
pretty_asop (DecafMinusEq) = "<-="
pretty_binop (DecafBinArithOp (DecafPlusOp)) = "+"
pretty_binop (DecafBinArithOp (DecafMinOp)) = "-"
pretty_binop (DecafBinArithOp (DecafMulOp)) = "*"
pretty_binop (DecafBinArithOp (DecafModOp)) = "%"
pretty_binop (DecafBinArithOp (DecafDivOp)) = "/"
pretty_binop (DecafBinRelOp (DecafLTOp)) = "<"
pretty_binop (DecafBinRelOp (DecafGTOp)) = ">"
pretty_binop (DecafBinRelOp (DecafGTEOp)) = ">="
pretty_binop (DecafBinRelOp (DecafLTEOp)) = "<="
pretty_binop (DecafBinEqOp (DecafEqOp)) = "=="
pretty_binop (DecafBinEqOp (DecafNEqOp)) = "!="
pretty_binop (DecafBinCondOp (DecafAndOp)) = "&&"
pretty_binop (DecafBinCondOp (DecafOrOp)) = "||"
pretty_arr_expr(DecafArrLoc ident expr) = ident ++ "[e]"
convertProgram :: DecafProgram -> Tree String
convertProgram p  = Node ( "Program") (Just children)
                    where
                      fs = map convertField (fields p)
                      ms = map convertMethod (methods p)
                      children = fs ++ ms

convertField :: DecafField -> Tree String
convertField (DecafVarField var@(DecafVar ty ident)) = convertVar var
convertField (DecafArrField arr@(DecafArr ty ident len)) = Node ( pretty_arr arr) Nothing

convertVar :: DecafVar -> Tree String
convertVar var@(DecafVar ty ident) = Node ( pretty_var var) Nothing

convertMethod :: DecafMethod -> Tree String
convertMethod meth@(DecafMethod ty ident args body) = Node( pretty_meth meth) (Just $ convertBlock body)

convertBlock :: DecafBlock -> [Tree String]
convertBlock (DecafBlock vars stms) = vs ++ ss
                                      where
                                        vs = map convertVar vars
                                        ss = map convertStm stms

convertMethodcall :: DecafMethodCall -> Tree String
convertMethodcall (DecafPureMethodCall ident args) = (Node "PureMethodcall") (Just $ [convertIdent ident] ++ (map convertExpr args))
convertMethodcall (DecafMethodCallout ident args) = (Node "MethodCallOut") (Just $ [convertStr ident] ++ (map convertCalloutArg args))

convertStm :: DecafStm -> Tree String
convertStm (DecafAssignStm loc op expr) = Node ( pretty_asop op) (Just $ [convertLoc loc] ++ [convertExpr expr])
convertStm (DecafMethodStm m@(DecafPureMethodCall ident args)) = convertMethodcall m
convertStm (DecafMethodStm m@(DecafMethodCallout ident args)) = convertMethodcall m
convertStm (DecafIfStm expr block elseblock) = Node ( "if") (Just $ [convertExpr expr] ++ convertBlock block ++ elseblock')
                                              where
                                                elseblock' = case convertElseBlock elseblock of
                                                                Just a -> a
                                                                Nothing -> []
convertStm (DecafForStm ident expr expr' block) = Node ( "for") (Just $ [convertIdent ident] ++ [convertExpr expr] ++ [convertExpr expr'] ++ convertBlock block)
convertStm (DecafRetStm expr) = Node ( "ret") $ convertRetExpr expr
convertStm (DecafBreakStm) = Node ( "brk") Nothing
convertStm (DecafContStm) = Node ( "cnt") Nothing
convertStm (DecafBlockStm block) = Node ( "blockStm") (Just $ convertBlock block)

convertRetExpr :: (Maybe DecafExpr) -> (Maybe [Tree String])
convertRetExpr (Just expr) = Just $ [convertExpr expr]
convertRetExpr Nothing = Nothing

convertElseBlock :: (Maybe DecafBlock) -> (Maybe [Tree String])
convertElseBlock (Just block) = Just $ convertBlock block
convertElseBlock Nothing = Nothing

convertCalloutArg :: DecafCalloutArg -> Tree String
convertCalloutArg (DecafCalloutArgExpr expr) = convertExpr expr
convertCalloutArg (DecafCalloutArgStr str) = convertStr str

convertIdent :: DecafIdentifier -> Tree String
convertIdent str = Node( "IDENT "++ str) Nothing

convertLoc :: DecafLoc -> Tree String
convertLoc var@(DecafVarLoc ident) = convertIdent ident
convertLoc arr@(DecafArrLoc ident expr) = Node ( pretty_arr_expr arr) (Just $ [convertExpr expr])

convertStr :: DecafString -> Tree String
convertStr str = Node ( "STR \""++str++"\"") Nothing

convertExpr :: DecafExpr -> Tree String
convertExpr (DecafExpr term expr') = Node "EXPR" (Just $ [convertTerm term] ++ [convertExpr' expr'])

convertExpr' :: Expr' -> Tree String
convertExpr' (Expr' binop term expr') = (Node $ pretty_binop binop) (Just $ [convertTerm term] ++ [convertExpr' expr'])
convertExpr' (EmptyExpr') = Nil

convertTerm :: Term -> Tree String
convertTerm (Term factor term') = (Node "TERM") (Just $ [convertFactor factor] ++ [convertTerm' term'])

convertTerm' :: Term' -> Tree String
convertTerm' (Term' binop factor term') = (Node $ pretty_binop binop) (Just $ [convertFactor factor] ++ [convertTerm' term'])
convertTerm' (EmptyTerm') = Nil

convertFactor :: Factor -> Tree String
convertFactor (DecafParenExpr' expr) = (Node "()") (Just [convertExpr expr])
convertFactor (DecafNotExpr' expr) = (Node "!") (Just [convertExpr expr])
convertFactor (DecafMinExpr' expr) = (Node "-") (Just [convertExpr expr])
convertFactor (DecafLocExpr' loc) = convertLoc loc
convertFactor (DecafMethodExpr' dmc) = convertMethodcall dmc
convertFactor (DecafLitExpr' dl) = convertLiteral dl

convertLiteral :: DecafLiteral -> Tree String
convertLiteral (DecafIntLit i) = Node (pretty_int i) Nothing
convertLiteral (DecafBoolLit b) = Node (show b) Nothing
convertLiteral (DecafStrLit s) = (Node s) Nothing
convertLiteral (DecafCharLit c) = (Node $ show c) Nothing
