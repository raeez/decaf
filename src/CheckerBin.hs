module Main where
import System.Environment
import Decaf.Checker
import Decaf.Parser

main = do args <- getArgs
          str <- readFile (head args)
          case ps program str of
            RSuccess prog -> 
                case (runChecker (checkProgram prog) ("", mkTree $SymbolTable [] GlobalBlock)) of
                  (_,(e,t)) -> putStrLn $ e ++ show t
            RError str -> putStrLn str