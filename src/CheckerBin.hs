module Main where
import System.Environment
import Decaf.Checker
import Decaf.Parser
import Decaf.Tokens

main = do args <- getArgs
          str <- readFile (head args)
          case ps program str of
            RSuccess prog -> 
                case (runChecker (checkProgram prog) ("", mkTree $SymbolTable [] GlobalBlock)) of
                  (_,(e,t)) -> putStrLn.unlines.(map (((head args)++":")++)).lines $ e
            RError str -> putStrLn str