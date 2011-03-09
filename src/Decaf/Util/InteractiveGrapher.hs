module Decaf.Util.InteractiveGrapher where
import System.Process
import Decaf.Data.GraphTree

graph z = do
    outputGraph z
    runCommand ("dot -Tpng ghci.dot -o ghci.png")
    runCommand ("open ghci.png")
    where
      graphText = renderGraph . buildGraph
      outputGraph a = putStrLn (graphText a) >> writeFile "ghci.dot" (graphText a)
