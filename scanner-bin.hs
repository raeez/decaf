module Main
where
import Scanner
import Data.List
import System.Environment

main :: IO ()
main = do
        args <- getArgs
        case args of
          [inputFile] -> Main.run f inputFile
          otherwise -> putStrLn "error, must specify the input file as the first and only arg"
        where
          f = frepl

run function inputFile = do
                           input <- readFile inputFile
                           writeFile outputFile $ function input
                           putStrLn $ function input
                           where
                             outputFile = inputFile ++ ".out"

frepl inp = unlines $ map showToken $ init $ getReport $ readTokens inp
