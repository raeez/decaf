module Main where
import Decaf
import Decaf.Util.InteractiveGrapher
import Decaf.RegisterAllocator

--t' =  (top . (runLabelCounter mkCounter) . numberTree . tree) t
main = do source <- readFile "test.c"
          let (e, p, t, formattedTree, formattedErrors) = getRSuccess $ check source "test.c"
          --graph $ generify t
          if length e > 0 then putStrLn formattedErrors else putStrLn ""
          let
              (a, b) = runRegisterCounter (numberTree $ tree t) (CounterState mkCounter)
              (lir, _) = runTranslator (translateProgram (top a) p) mkNamespace
--          putStrLn $ pp $ translateCFG . convertProgram $ lir
              prog = translateCG . convertProgram $ lir
              results = allocateRegisters t prog 
          mapM (putStrLn.(++"\n").show) results
