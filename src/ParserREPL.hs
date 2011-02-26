module Main
where
import Data.Graph.Inductive
import Data.GraphViz
import Decaf.Parser
import Decaf.AST

main :: IO ()
main = do
        putStrLn "Decaf Parser REPL"
        putStrLn "Enter your input program at the prompt\n"
        replEval

replEval :: IO ()
replEval = do
            inp <- getLine
            case ps expr inp of
              RSuccess a -> (putStrLn $ printDotGraph $ graphToDot graphParams ( graph a)) >> (putStrLn $ "\n\n" ++ (put_expr 0 a))
              RError e -> putStrLn $ "Error:\n" ++ e
            replEval

graph d = let (nodes, edges) = convertTree d in
              mkGraph nodes edges :: Gr String String

convertTree d  = ( [    (1, "Collect underpants"),
                        (3, "Profit")   ],           [ (1, 3, "?") ] )
--class GraphViz a where
    --toGraphViz :: a -> ((Int, String), [(Int, Int, String)])

--instance DecafExpr where
    --toGraphViz n (DecafExpr term expr') = ([this] ++ children, leaves)
                                          --where
                                            --this = (n, 
                                            --children = []
                                            --leaves = 
--instance Expr' where
    --toGraphViz n (Expr' term expr') = ([this] ++ children, leaves) 
                                      --where
                                          --children = 
                                          --leaves = 

--flatten n (DecafExpr term expr') = []
--flatten n (Expr' binop term expr') = []
--flatten n (EmptyExpr') = []
--flatten n (Term factor term') = []
--flatten n (Term' binop factor term) = []
--flatten n (EmptyTerm') = []
--flatten n (DecafParenExpr' de) = []
--flatten n (DecafNotExpr' dne) = []
--flatten n (DecafMinExpr' dme) = []
--flatten n (DecafLocExpr' dle) = []
--flatten n (DecafMethodExpr' dme) = []
--flatten n (DecafLitExpr' dle) = []
--flatten n (DecafBinArithOp dbo) = []
--flatten n (DecafBinRelOp dbo) = []
--flatten n (DecafBinEqOp dbo) = []
--flatten n (DecafBinCondOp dco) = []
--flatten n (DecafPlusOp dpo) = []
--flatten n (DecafMinOp dmo) = []
--flatten n (DecafMulOp dmo) = []
--flatten n (DecafDivOp dvo) = []
--flatten n (DecafModOp dmo) = []
--flatten n (DecafLTOp dlto) = []
--flatten n (DecafGTOp dgto) = []
--flatten n (DecafLTEOp dlteo) = []
--flatten n (DecafGTEOp dgteo) = []
--flatten n (DecafEqOp deo) = []
--flatten n (DecafNEqOp dneo) = []
--flatten n (DecafAndOp dao) = []
--flatten n (DecafOrOp doo) = []

graphParams :: GraphvizParams String String () String
graphParams = nonClusteredParams {
		globalAttributes = ga,
		fmtNode = fn,
		fmtEdge = fe
	}
	where
		ga = [
			GraphAttrs [
				RankDir FromLeft,
				(BgColor . X11Color) Transparent
				],
			NodeAttrs [
				Shape BoxShape,
				(FillColor . X11Color) White,
				Style [SItem Filled []]
				]
			]

		fn (n,l) = [(Label . StrLabel) l]
		fe (f,t,l) = [(Label . StrLabel) l]
