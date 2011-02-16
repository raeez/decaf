module Main
where
import Scanner
import Parser
import Data.List
import System.Environment

main :: IO ()
main = do
        args <- getArgs
        case args of
          [inputFile] -> Main.run f inputFile
          otherwise -> putStrLn "error, must specify the input file as the first and only arg"
        where
          f = parser

run function inputFile = do
                           input <- readFile inputFile
                           case (numLexErrorsIn input == 0) of
                             False -> (putStrLn ("Decaf Compiler\nFile " ++ inputFile ++ " has lex errors!\nToken Stream:\n\n")) >> (scprint input)
                             otherwise -> (writeFile outputFile $ function input) >> (putStrLn $ function input)
                             where
                               outputFile = inputFile ++ ".out"
