module Main where
import System.Environment


main :: IO ()
main = do
        args <- getArgs
        case args of
          [inputFile] -> do x <- readFile inputFile; putStrLn $ show x
          otherwise -> putStrLn "error, must specify the input file as the first and only arg"
