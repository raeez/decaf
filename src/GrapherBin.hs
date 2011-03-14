module Main where
import System.Process
import System.Environment
import System.Exit
import Decaf.Data.GraphTree
import Decaf.Data.Zipper
import Decaf.Util.Report
import Decaf.IR.Class
import Decaf.IR.AST
import Decaf.Parser
import Decaf.Checker hiding (check)

basename :: String -> String
basename f = fst $ break (=='.') f

buildFilename :: String -> Int -> String -> String
buildFilename f i ext = basename f ++ "." ++ show i ++ "." ++ ext

convertToPNG :: String -> String -> IO ()
convertToPNG graphFile imageFile = runCommand ("dot -Tpng " ++ graphFile ++ " -o" ++ imageFile) >> putStrLn ""

main :: IO ()
main = do
        args <- getArgs
        case args of
          [inputFile] -> readFile inputFile >>= parse inputFile
          _           -> putStrLn "error, must specify the input file as the first and only arg"

parse :: String -> String -> IO ()
parse inputFile input =
          case ps program input of
              RSuccess a -> print a  >> outputGraph a >> convertToPNG graphFile imageFile >> (check inputFile input)
              RError e -> putStrLn ("Error:\n" ++ show e) >> exitFailure
          where
            graphText =  renderGraph . buildGraph . treeify
            outputGraph a = putStrLn (graphText a) >> writeFile graphFile (graphText a)
            graphFile = buildFilename inputFile 0 "out"
            imageFile = buildFilename inputFile 0 "png"

check :: String -> String -> IO ()
check inputFile input =
          case checkFile input inputFile of
            RSuccess (_, t, dT, dE) -> print dT >> print dE >> outputGraph t >> convertToPNG graphFile imageFile >> exitSuccess
            RError e -> putStrLn ("Error:\n" ++ show e) >> exitFailure
          where
            graphText = renderGraph . buildGraph . generify
            outputGraph a = putStrLn (graphText a) >> writeFile graphFile (graphText a)
            graphFile = buildFilename inputFile 1 "out"
            imageFile = buildFilename inputFile 1 "png"
