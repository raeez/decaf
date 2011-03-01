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
          [inputFile] -> (readFile inputFile) >>= (parse inputFile)
          otherwise -> putStrLn "error, must specify the input file as the first and only arg"

parse inputFile input = do
    if numLexErrorsIn tokens > 0
      then (let errorMessage = ("Decaf Compiler\nFile " ++
                                      inputFile ++
                                      " has lex errors!\nToken Stream:\n\n")
           in putStrLn errorMessage >> (putStrLn . scprint) input >> exitFailure)
       else (writeFile outputFile output >> putStrLn output >> (case parserErrors of
                                                                          False ->exitSuccess
                                                                          True -> exitFailure))
     where
       tokens = scanner input
       outputFile = inputFile ++ ".out"
       output = parser input
       parserErrors = containsErrors $ qs program input
