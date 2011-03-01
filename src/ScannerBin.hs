module Main
where
import Decaf.Scanner
import System.Environment
import System.Exit

main :: IO ()
main = do
        args <- getArgs
        case args of
          [inputFile] -> (readFile inputFile) >>= (scan inputFile)
          otherwise -> putStrLn "error, must specify the input file as the first and only arg"

scan inputFile input = do
    if numLexErrorsIn tokens >  0
      then (let errorMessage = ("Decaf Compiler\nFile " ++ input ++ " has lex errors!\nToken Stream:\n\n")
           in putStrLn errorMessage >> putStrLn output >> exitFailure)
      else (writeFile outputFile output >> putStrLn output >> exitSuccess)
    where
      tokens = scanner input
      numErrors = numLexErrorsIn tokens
      outputFile = inputFile ++ ".out"
      output = formatScannerOutput tokens
