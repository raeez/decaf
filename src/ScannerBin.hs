module Main
where
import Decaf.Scanner
import System.Environment
import System.Exit

main :: IO ()
main = do
        args <- getArgs
        case args of
          [s, inputFile] | s == "-debug" -> 
                  do str <- (readFile inputFile) 
                     let res = scan str
                     putStrLn.snd $ res
                     exitSuccess
          [inputFile] -> 
              do str <- readFile inputFile
                 let res = scan str
                 if fst res
                  then exitSuccess
                  else (putStrLn.snd $ res) >> exitFailure

          otherwise -> putStrLn "Invalid command line input" >> exitFailure

scan input = 
    let tokens = scanner input
        output = formatScannerOutput tokens in
    if numLexErrorsIn tokens > 0 
    then (False, output)
    else (True, output)
     
