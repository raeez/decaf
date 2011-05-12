module Main where
import System.Environment
import System.Exit
import Decaf

main :: IO ()
main = do args <- getArgs
          case args of
            [s, f] | s == "-debug" ->
                    do str <- readFile f
                       check True str f
            [f] -> do str <- readFile f
                      check False str f
            _ -> putStrLn "Invalid command line input" >> exitFailure

check :: Bool -> String -> String -> IO ()
check d input inputFile = do
        case checkFile input inputFile of
          RSuccess (_, _, dT, dE) -> printDebug dT >> putStrLn dE >> exitSuccess
          RError e -> putStrLn ("Error:\n" ++ show e) >> exitFailure
        where
          printDebug t = do
                          if d then putStrLn t else return ()
