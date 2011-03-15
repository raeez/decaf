module Main where
import Decaf
import Decaf.Util.InteractiveGrapher

--t' =  (top . (runLabelCounter mkCounter) . numberTree . tree) t
main = do source <- readFile "test.c"
          let (e, p, t) = getRSuccess $ check source
          --graph $ generify t
          let
              (a, b) = runRegisterCounter (numberTree $ tree t) (CounterState mkCounter)
              (lir, _) = runTranslator (translateProgram (top a) p) mkNamespace
          putStrLn $ pp $ lir
