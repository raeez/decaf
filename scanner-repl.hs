module Main
where
import Scanner
import Data.List
import System.Environment

main :: IO ()
main = do

        putStrLn "Decaf Scanner REPL"
        putStrLn "Enter your token stream at the prompt\n"
        replEval

replEval = do
            inp <- getLine
            putStrLn $ unlines . map showToken . readTokens $ inp
            replEval
