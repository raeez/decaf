module Main where
import Decaf
import Decaf.Util.InteractiveGrapher
import Decaf.RegisterAllocator
import Decaf.IR.ASM
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
          do if length e > 0
               then putStrLn formattedErrors >> exitFailure
               else do let (numberedTable, 
                            CounterState (LabelCounter rc _ _ _)) =
                                runRegisterCounter (numberTree $ tree t) (CounterState mkCounter)
                           (lir, _) = runTranslator (translateProgram (top numberedTable) p) (mkNamespace rc)
                       if debug
                         then (putStrLn $ pp $ translateCFG . convertProgram $ lir)
                         else putStrLn ""
                       let prog = (translateCFG . convertProgram) lir
                           (prog', c, results) = allocateRegisters t prog 
                           assembler = programAssembler (content numberedTable) prog'
                           (prog'', state') = runAssembler assembler mkAssemblerState
                           asmout = nasm prog''
                       writeFile ((fst $ break (=='.') filename) ++ ".asm") asmout
                       putStrLn asmout
                       exitSuccess
