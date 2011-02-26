module Main
where
import Decaf.Scanner

main :: IO ()
main = do

        putStrLn "Decaf Scanner REPL"
        putStrLn "Enter your token stream at the prompt\n"
        replEval

replEval :: IO ()
replEval = do
            inp <- getLine
            putStrLn $ formatScannerOutput $ eatFirst inp
            replEval
