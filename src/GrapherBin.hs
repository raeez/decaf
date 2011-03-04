module Main where
import Data.GraphViz
import System.Process
import System.Environment
import System.Exit
import Decaf.AST
import Decaf.Data.GraphTree
import Decaf.Parser
import Decaf.Util

main :: IO ()
main = do
        args <- getArgs
        case args of
          [inputFile] -> readFile inputFile >>= parse inputFile
          _           -> putStrLn "error, must specify the input file as the first and only arg"

parse :: String -> String -> IO ()
parse inputFile input =
          case ps program input of
              RSuccess a -> print a  >> outputGraph a >> convertToPNG >> exitSuccess
              RError e -> putStrLn ("Error:\n" ++ show e) >> exitFailure
          where
            outputFile = inputFile ++ ".out"
            graphText =  printDotGraph . graphToDot graphParams . buildGraph . treeify
            outputGraph a = print (graphText a) >> writeFile outputFile (graphText a)
            convertToPNG = runCommand ("dot -Tpng " ++ outputFile ++ " -o" ++ outputFile ++ ".png") >> putStrLn ""

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

    fn (_, l) = [(Label . StrLabel) l]
    fe (_, _,l) = [(Label . StrLabel) l]
