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
            outputGraph a = (putStrLn $ show $ (extract . numberTree 0 . treeify) a) >> (putStrLn $ graphText a) >> (writeFile outputFile $ graphText a) 
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
buildGraph program = mkGraph nodes edges :: Gr String String
                   where
                    (nodes, edges) = (extract . numberTree 0 . treeify) program
