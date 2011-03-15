module Main where
import Decaf
import Decaf.Util.InteractiveGrapher
import Decaf.RegisterAllocator
import Decaf.IR.ASM

--t' =  (top . (runLabelCounter mkCounter) . numberTree . tree) t
main = do source <- readFile "test.c"
          let (e, p, t, formattedTree, formattedErrors) = getRSuccess $ check source "test.c"
          --graph $ generify t
          if length e > 0 then putStrLn formattedErrors else putStrLn ""
          let (numberedTable, CounterState (LabelCounter rc _ _ _)) = runRegisterCounter (numberTree $ tree t) (CounterState mkCounter)
              (lir, _) = runTranslator (translateProgram (top numberedTable) p) (mkNamespace rc)
          putStrLn $ pp $ translateCFG . convertProgram $ lir
          let prog = (translateCFG . convertProgram) lir
              results = allocateRegisters t prog 
          --mapM (putStrLn.(++"\n").show) results
          (putStrLn . intelasm) prog
