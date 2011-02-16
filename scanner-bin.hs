module Main
where
import Scanner
import Data.List
import System.Environment
import System.Exit

main :: IO ()
main = do
        args <- getArgs
        case args of
          [inputFile] -> (readFile inputFile) >>= (execute f inputFile)
          otherwise -> putStrLn "error, must specify the input file as the first and only arg"
        where
          f = frepl

execute function inputFile input = do
   case (numLexErrorsIn input == 0) of
     False -> (putStrLn ("Decaf Compiler\nFile " ++ input ++ " has lex errors!\nToken Stream:\n\n")) >> (scprint input) >> exitFailure
     True -> (writeFile outputFile output) >> (putStrLn output) >> exitSuccess
     where
       outputFile = inputFile ++ ".out"
       output = function input

frepl inp = formattedOutput $ eatFirst inp
