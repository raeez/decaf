module Main where
import System.Environment
import System.Exit
import Decaf.Checker
import Decaf.Parser
import Decaf.Tokens

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
                   putStrLn.snd $ output
                   checkOutput output

            other -> putStrLn "Invalid command line input" >> exitFailure

checkFile str file =
    case ps program str of
      RSuccess prog -> 
          case (runChecker (checkProgram prog) ("", mkTree $SymbolTable [] GlobalBlock)) of
            (_,(e,t)) -> ((show prog)++"\n\n"++(show t),init.unlines.(map ((file++":")++)).lines $ e)
      RError str -> (str,str)

checkOutput output = 
    if length (snd output) > 0
    then exitSuccess
    else exitFailure
