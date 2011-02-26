module Main
where
import Decaf.Parser

main :: IO ()
main = do

        putStrLn "Decaf Parser REPL"
        putStrLn "Enter your input program at the prompt\n"
        replEval

replEval :: IO ()
replEval = do
            inp <- getLine
            putStrLn $ ps expr inp
            replEval
