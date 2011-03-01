module Main
where
import Decaf.Scanner
import Decaf.Parser
import Data.List
import Text.Regex.Posix
import System.Environment
import System.Exit

main :: IO ()
main = do
        args <- getArgs
        case args of
          [s, inputFile] | s == "-debug" -> (readFile inputFile) >>= (parse inputFile True)
          [inputFile] -> (readFile inputFile) >>= (parse inputFile False)
          otherwise -> putStrLn "Invalid command line input" >> exitFailure

parse inputFile debug input  = 
    if numLexErrorsIn tokens > 0
        then (let errorMessage = ("Decaf Compiler\nFile " ++
                                      inputFile ++
                                      " has lex errors!\nToken Stream:\n\n")
             in putStrLn errorMessage >> (putStrLn . scprint) input >> exitFailure)
        else case parserErrors of
           False -> if debug
                    then putStrLn output >> exitSuccess
                    else  exitSuccess
           True -> putStrLn output >> exitFailure
     where
       tokens = scanner input
       outputFile = inputFile ++ ".out"
       output = parser input
       parserErrors = containsErrors $ qs program input
