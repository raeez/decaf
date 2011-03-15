module Main where
import Decaf
import Decaf.Util.InteractiveGrapher
import Decaf.RegisterAllocator
import Decaf.IR.ASM

import System.Environment

--t' =  (top . (runLabelCounter mkCounter) . numberTree . tree) t
main = do input <- getArgs
          source <- readFile (input !! 0)
          let (e, p, t, formattedTree, formattedErrors) = getRSuccess $ check source "test.c"
          --graph $ generify t
          if length e > 0 then putStrLn formattedErrors else putStrLn ""
          let (numberedTable, CounterState (LabelCounter rc _ _ _)) = runRegisterCounter (numberTree $ tree t) (CounterState mkCounter)
              (lir, _) = runTranslator (translateProgram (top numberedTable) p) (mkNamespace rc)
          putStrLn $ pp $ translateCFG . convertProgram $ lir
          let prog = (translateCFG . convertProgram) lir
              (prog', c, results) = allocateRegisters t prog 

          putStrLn.pp $ prog' 
          putStrLn.show $ c
          mapM (putStrLn.show) results

          --(putStrLn . intelasm) prog
