module Main where
import Decaf
import Decaf.RegisterAllocator
import System.Environment
import System.Exit
import System.Process

import Loligoptl

import System.Console.GetOpt
import Data.Maybe
import Debug.Trace

data Flag = Debug
          | Graph
          | Opt
          deriving (Eq, Show)

options :: [OptDescr Flag]
options =
 [ Option ['v'] ["debug"] (NoArg Debug) "output debug information"
 , Option ['g'] ["graph"] (NoArg Graph) "output various dot graphs"
 , Option ['o'] ["opt"]   (NoArg Opt)   "enable optimization [all|cse]"

 ]
compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = 
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (o, n)
      (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: ic [OPTION...] files..."

main :: IO ()
main = do args <- getArgs
          argv <- compilerOpts args
          case argv of
              (o, n) -> do let file = last n
                               debug = Debug `elem` o
                               graph = Graph `elem` o
                               allopt = Opt `elem` o
                               cseopt = Opt `elem` o
                           if allopt
                             then putStrLn "optimizations on"
                             else putStrLn "no optimizations active"
                           if debug
                             then putStrLn "debug on"
                             else putStrLn "no debug output active"
                           str <- readFile file
                           let compOpts = CompilationOpts debug graph [allopt, cseopt]
                           compile compOpts str file
              _ -> putStrLn "Invalid command line input" >> exitFailure

type OutputDebugInfo = Bool
type OutputGraphInfo = Bool
type OptimizationFlags = [Bool]
type SourceText = String
type FileName = String

data CompilationOpts = CompilationOpts
    { debug :: Bool
    , graph :: Bool
    , optopts :: [Bool]
    }

parse :: CompilationOpts
        -> SourceText
        -> FileName
        -> IO DecafProgram
parse chosen source filename =
          case ps program source of
              RSuccess a ->
                do
                  if debug chosen
                    then putStrLn $ "Parse output\n:" ++ show a
                    else return ()

                  if graph chosen
                    then (outputGraph a >> convertToPNG graphFile imageFile)
                    else return ()

                  return a

              RError e -> putStrLn ("Error:\n" ++ show e) >> exitFailure
          where
            graphText =  renderGraph . buildGraph . treeify
            outputGraph a = putStrLn (graphText a) >> writeFile graphFile (graphText a)
            graphFile = buildFilename filename 0 "out"
            imageFile = buildFilename filename 0 "png"


check :: CompilationOpts
        -> SourceText
        -> FileName
        -> IO SymbolTree
check chosen source filename =
          case checkFile source filename of
            RSuccess (e, t, formattedTree, formattedErrors) ->
              do
                if debug chosen
                  then do putStrLn $ "Formatted Tree\n: " ++ formattedTree
                          putStrLn $ "Formatted Errors\n: " ++ formattedErrors
                  else return ()

                if graph chosen
                  then outputGraph t >> convertToPNG graphFile imageFile
                  else return ()

                if length e > 0
                  then putStrLn formattedErrors >> exitFailure
                  else return ()

                return t

            RError e -> putStrLn ("Error:\n" ++ show e) >> exitFailure
          where
            graphText = renderGraph . buildGraph . generify
            outputGraph a = putStrLn (graphText a) >> writeFile graphFile (graphText a)
            graphFile = buildFilename filename 1 "out"
            imageFile = buildFilename filename 1 "png"

compile :: CompilationOpts
            -> SourceText -> FileName -> IO ()
compile chosen source filename =
  do parsed_program <- parse chosen source filename
     checked_symbols <- check chosen source filename

     let (numberedTable, CounterState (LabelCounter rc _ mc _)) =
             runRegisterCounter (numberTree $ tree checked_symbols)
                                 (CounterState mkCounter)
         -- ^ run the register counter
         --
         control_flow_graph = graphProgram (top numberedTable) parsed_program (rc + mc)
         -- ^ convert to control flow graph
 
     (optimized, dominatorTree, dominanceFrontiers) <- optimize control_flow_graph

     let control_flow_graph' = if or (optopts chosen)
                              then optimized
                              else control_flow_graph

        -- ^ for now, optimize if any opt flags are present
        -- TODO pick opts on a per-flag basis


         program = LIRProgram (LIRLabel "" 0)
                              [LIRUnit (LIRLabel "" 0)
                              (graphToLIR control_flow_graph')]
        -- ^ translate back to LIR

         assembler = programAssembler (content numberedTable) program
         -- ^ pre-assembly monad

         (assembler', _) = runAssembler assembler mkAssemblerState
         -- ^ translate to pre-assembly
 
         asmout = nasm assembler'
         -- ^ translate to NASM assembler syntax

     if graph chosen
       then do outputGraph (show dominatorTree)
               convertToPNG graphFile imageFile
       else return ()

     writeFile newFile asmout
     putStrLn asmout
     putStrLn $ pp program
     --putStrLn $ "Finally DF:\n" ++ show dominanceFrontiers
     --putStrLn "-----"
     exitSuccess
  where
    newFile = (fst $ break (=='.') filename) ++ ".asm"
    outputGraph a = putStrLn a >> writeFile graphFile a
    graphFile = buildFilename filename 2 "out"
    imageFile = buildFilename filename 2 "png"

type M = CheckingFuelMonad (SimpleUniqueMonad)

optimize :: LIRGraph C C -> IO (LIRGraph C C, DominatorTree, DominanceFrontiers)
optimize g = do
  let g' = runSimpleUniqueMonad $ runWithFuel infiniteFuel (cseOpt g)
      (df, dt) = runSimpleUniqueMonad $ runWithFuel infiniteFuel (ssa g')
  return (g', dt, df)

cseOpt :: LIRGraph C C -> M (LIRGraph C C)
cseOpt g = do
  let entry = LIRLabel "main" (-1)
  (g', facts, _) <- analyzeAndRewriteFwd csePass
                                              (JustC [entry])
                                              g
                                              (mapSingleton entry cseTop)
  return g'

ssa :: LIRGraph C C -> M (DominanceFrontiers, DominatorTree)
ssa g = do
    let entry = LIRLabel "main" (-1)
    (_, domFacts, _) <- analyzeAndRewriteFwd domPass
                                                (JustC [entry])
                                                g
                                                (mapSingleton entry domEntry)

    (_, varFacts, _) <- analyzeAndRewriteFwd varPass
                                                (JustC [entry])
                                                g
                                                (mapSingleton entry varEntry)
    let dominanceFrontiers = mkDF g domFacts

    return dominanceFrontiers

basename :: String -> String
basename f = fst $ break (=='.') f

buildFilename :: String -> Int -> String -> String
buildFilename f i ext = basename f ++ "." ++ show i ++ "." ++ ext

convertToPNG :: String -> String -> IO ()
convertToPNG graphFile imageFile = runCommand ("dot -Tpng " ++ graphFile ++ " -o" ++ imageFile) >> putStrLn ""
