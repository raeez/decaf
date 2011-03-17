module Main where
import Decaf
import Decaf.Util.InteractiveGrapher
import Decaf.RegisterAllocator
import Decaf.IR.ASM
import System.Environment
import System.Exit

main = do input <- getArgs
          let (filename:fs) = input
          source <- readFile filename
          do case check source filename of
              RError s -> putStrLn s
              RSuccess (e, p, t, formattedTree, formattedErrors) ->
                  do if length e > 0
                       then putStrLn formattedErrors >> exitFailure
                       else do let (numberedTable, CounterState (LabelCounter rc _ _ _)) = runRegisterCounter (numberTree $ tree t) (CounterState mkCounter)
                                   (lir, _) = runTranslator (translateProgram (top numberedTable) p) (mkNamespace rc)
                               putStrLn $ pp $ translateCFG . convertProgram $ lir
                               let prog = (translateCFG . convertProgram) lir
                                   (prog', c, results) = allocateRegisters t prog 
                                   asm =  ((intelasm . content) numberedTable) ++ (intelasm prog')
                               writeFile ((fst $ break (=='.') filename) ++ ".asm") asm
                               exitSuccess
