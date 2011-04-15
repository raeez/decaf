module Main where
import Decaf
import Decaf.RegisterAllocator
import System.Environment
import System.Exit

main :: IO ()
main = do args <- getArgs
          case args of
            [s, f] | s == "-debug" ->
                    do str <- readFile f
                       compile True str f
            [f] -> do str <- readFile f
                      compile False str f
            _ -> putStrLn "Invalid command line input" >> exitFailure

compile :: Bool -> String -> String -> IO ()
compile debug source filename =
  do case check source filename of
      RError s -> putStrLn s
      RSuccess (e, p, t, formattedTree, formattedErrors) ->
          do if debug
                then (putStrLn $ "formattedTree: " ++ formattedTree)
                else (return ())
             if length e > 0
               then putStrLn formattedErrors >> exitFailure
               else do let (numberedTable, 
                            CounterState (LabelCounter rc _ mc _)) =
                                runRegisterCounter (numberTree $ tree t)
                                                   (CounterState mkCounter)
                       let gProg = graphProgram (top numberedTable) p (rc + mc)
                           prog = LIRProgram (LIRLabel "" 0) [LIRUnit (LIRLabel "" 0) (graphToLIR gProg)]
                           assembler = programAssembler (content numberedTable) prog
                           (prog', _) = runAssembler assembler mkAssemblerState
                           asmout = nasm prog'
                       writeFile newFile asmout
                       putStrLn asmout
                       exitSuccess
  where
    newFile = (fst $ break (=='.') filename) ++ ".asm"
