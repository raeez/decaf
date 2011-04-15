module Main where
import Decaf
import Decaf.RegisterAllocator
import System.Environment
import System.Exit

import Loligoptl
import Decaf.TRCSE
import Decaf.HooplNodes

import System.Console.GetOpt
import Data.Maybe

data Flag = Debug
          | Opt String
          deriving (Eq, Show)

options :: [OptDescr Flag]
options =
 [ Option ['v']     ["debug"] (NoArg Debug)       "output debug information"
 , Option ['a']     ["opt"]  (OptArg allOpt "opt all")  "enable optimization [all|cse]"
 , Option ['c']     ["opt"]  (OptArg cseOpt "opt cse")  "enable optimization [all|cse]"
 ]

cseOpt, allOpt :: Maybe String -> Flag
cseOpt = Opt . fromMaybe "cse"
allOpt = Opt . fromMaybe "all"

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
                               allopt = Opt "all" `elem` o
                               cseopt = Opt "cse" `elem` o
                           str <- readFile file
                           compile debug [allopt, cseopt] str file
              _ -> putStrLn "Invalid command line input" >> exitFailure

compile :: Bool -> [Bool] -> String -> String -> IO ()
compile debug optopts source filename =
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
                           mainlab = LIRLabel "main" (-1)
                           gProg' :: DecafGraph C C
                           gProg' = fst.fst $ runLFM (analyzeAndFwdRewrite csePass [mainlab]
                                                gProg (mapSingleton mainlab (factBottom.fpLattice $ (csePass :: FwdPass LolMonad Node CSEFact))))
                                                mkInfiniteFuel
                           --optimized = if or optopts
                                        --then optimize gProg
                                        --else gProg
                           prog = LIRProgram (LIRLabel "" 0) [LIRUnit (LIRLabel "" 0) (graphToLIR gProg')]
                           assembler = programAssembler (content numberedTable) prog
                           (prog', _) = runAssembler assembler mkAssemblerState
                           asmout = nasm prog'
                       writeFile newFile asmout
                       putStrLn asmout
                       exitSuccess
  where
    newFile = (fst $ break (=='.') filename) ++ ".asm"
