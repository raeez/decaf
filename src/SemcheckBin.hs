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
          [inputFile] -> (readFile inputFile) >>= (execute inputFile)
          otherwise -> putStrLn "error, must specify the input file as the first and only arg"

execute inputFile input = do
    case (numLexErrorsIn input == 0) of
       False -> let errorMessage = ("Decaf Compiler\nFile " ++
                                      inputFile ++
                                      " has lex errors!\nToken Stream:\n\n") in
                putStrLn errorMessage >> scprint input >> exitFailure
       otherwise -> writeFile outputFile output >> putStrLn output >> (case parserErrors of
                                                                          False ->exitSuccess
                                                                          True -> exitFailure)
     where
       outputFile = inputFile ++ ".out"
       output = parser input
       parserErrors = containsErrors $ qs program input
