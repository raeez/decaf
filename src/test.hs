module Main where
import Decaf
import Decaf.Util.InteractiveGrapher
import Decaf.RegisterAllocator
import Decaf.IR.ASM
import System.Environment

main = do input <- getArgs
          let fileName = input !! 0
          source <- readFile fileName
          let (e, p, t, formattedTree, formattedErrors) = getRSuccess $ check source fileName
          --graph $ generify t
          if length e > 0 then putStrLn formattedErrors else putStrLn ""
          let (numberedTable, CounterState (LabelCounter rc _ _ _)) = runRegisterCounter (numberTree $ tree t) (CounterState mkCounter)
              (lir, _) = runTranslator (translateProgram (top numberedTable) p) (mkNamespace rc)
          putStrLn $ pp $ translateCFG . convertProgram $ lir
          let prog = (translateCFG . convertProgram) lir
              (prog', c, results) = allocateRegisters t prog 

          --putStrLn.pp $ prog' 
          ---putStrLn.show $ c
          --mapM (putStrLn.show) results
          let asm =  ((intelasm . content) numberedTable) ++ (intelasm prog)
          putStrLn asm
          writeFile ((fst $ break (=='.') fileName) ++ ".asm") asm

