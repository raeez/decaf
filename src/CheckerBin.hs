module Main where
import System.Environment
import System.Exit
import Decaf.Checker

main :: IO ()
main = do args <- getArgs
          case args of
            [s, f] | s == "-debug" ->
                    do str <- readFile f
                       let output = checkFile str f
                       putStrLn.fst $ output
                       checkOutput output

            [f] ->
                do str <- readFile f
                   let output = checkFile str f
                   if not (null (snd output))
                    then (putStrLn.snd $ output) >> exitFailure
                    else exitSuccess

            _ -> putStrLn "Invalid command line input" >> exitFailure

checkOutput :: (String, String) -> IO ()
checkOutput output =
    if length (snd output) > 0
    then exitSuccess
    else exitFailure
