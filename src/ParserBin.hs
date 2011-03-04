module Main
where
import System.Environment
import System.Exit
import Decaf.Scanner
import Decaf.Parser

main :: IO ()
main = do
        args <- getArgs
        case args of
          [s, inputFile]
              | s == "-debug" -> readFile inputFile >>= parse inputFile True
          [inputFile] -> readFile inputFile >>= parse inputFile False
          _           -> putStrLn "Invalid command line input" >> exitFailure

parse :: String -> Bool -> String -> IO ()
parse inputFile debug input
    | numLexErrorsIn tokens > 0 =
        let errorMessage = "Decaf Compiler\nFile " ++
                                inputFile ++
                                " has lex errors!\nToken Stream:\n\n"
        in putStrLn errorMessage >> (putStrLn . scprint) input >> exitFailure

    | otherwise = if parserErrors
                  then putStrLn output >> exitFailure
                  else if debug
                       then putStrLn output >> exitSuccess
                       else  exitSuccess
     where
       tokens = scanner input
       output = parser input
       parserErrors = containsErrors $ qs program input
