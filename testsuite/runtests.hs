module Main where
import Test.HUnit
import Decaf
import Decaf.RegisterAllocator
import Decaf.IR.ASM
import GHC.IO.Exception
import System.Cmd




-------------------------------------
-- program to be tested
scanner_ :: String -> Bool
scanner_ i = if numLexErrorsIn (scanner i) > 0 then False else True

parser_ :: String -> Bool
parser_ i = case ps program i of
        RError _ -> False
        RSuccess _ -> True

semchecker_ :: String -> Bool                      
semchecker_  = checker 


-- cogen content, return program output
cogen_ :: String -> String
cogen_ source = 
       let (e, p, t, formattedTree, formattedErrors) = getRSuccess $ check source "DummyFilenname"
           (numberedTable, CounterState (LabelCounter rc _ _ _)) = runRegisterCounter (numberTree $ tree t) (CounterState mkCounter)
           (lir, _) = runTranslator (translateProgram (top numberedTable) p) (mkNamespace rc)
           prog = (translateCFG . convertProgram) lir
           (prog', c, results) = allocateRegisters t prog 
           asm =  ((intelasm . content) numberedTable) ++ (intelasm prog)
	in asm




-- load
readresult :: IO String 
readresult = do
	   x <- readFile "testout"
	   return x

-- run
coderun :: String -> IO (Maybe String)
coderun str = do
	 writeFile "test.s" str 
	 rawSystem "gcc" ["test.s", "-o", "test"]
	 rawSystem "rm" ["-rf", "testout"]
	 x <- rawSystem "./run.sh" ["test", "testout"]
	 case x of 
	      ExitSuccess -> do {
	      		     	x <- readresult;
	      		     	return $ Just x
	      		     }
	      _ -> return $ Nothing


-- runmany
manyrun :: [String] -> IO [Maybe String]
manyrun ss = do
	x <- sequence (map coderun ss)
	return x









------------------------------------
-- test programs

makeFileNames:: String -> Int -> Int -> [String]
makeFileNames prefix s e = map (\x->prefix++(g x)++".dcf") [s..e]
    where g x | x > 9  = show x
              | otherwise = "0"++(show x)

makeLegals:: Int -> Int -> [String]
makeLegals = makeFileNames "legal-"

makeIllegals :: Int -> Int -> [String]
makeIllegals = makeFileNames "illegal-"


readTests :: [String] -> Bool -> (String -> Bool) -> IO Test
readTests names expected func =
    do
      files <- sequence (map readFile names)
      return $TestList.(map (uncurry TestLabel)) $ zip names (map (\cont -> expected ~=? (func cont)) files)


-- read tests and their .out file      : Finish me 
readTestsRuntimeError :: [String] -> (String -> String) -> (String -> IO (Maybe String)) -> IO Test
readTestsRuntimeError names func run =
    do
      files <- sequence (map readFile names)
      let asms = map func files 
      filesout <- sequence (map run asms)
      let tests = map (\x -> Nothing ~=? x) filesout    -- fix Func, func need to go through gcc
      return $TestList.(map (uncurry TestLabel)) $ zip names tests

      
      
-- read tests and their .out file      : Finish me 
readTestsWithOut :: [String] -> (String -> String) -> (String -> IO (Maybe String)) -> IO Test
readTestsWithOut names func run =
    do
      files <- sequence (map readFile $ map (\x -> x++".dcf") names)
      outs <- sequence (map readFile $ map (\x -> x++".out") names)
      let asms = map func files 
      filesout <- sequence (map run asms)
      let tests = map (\x -> (Just $ snd x) ~=? (fst x)) $ zip filesout outs   -- fix Func, func need to go through gcc
      return $TestList.(map (uncurry TestLabel)) $ zip names tests
      
      

-- | read test from a list
readTList :: String -> IO [String]
readTList ln = readFile ln >>= return . lines

-- add path 
addPath :: String -> [String] -> [String]
addPath path = map (\x -> path++"/"++x)

-- add extension
addExt :: String -> [String] -> [String]
addExt ext = map (\x -> x++"."++ext)



main:: IO Counts
main = do

  putStrLn "Scanner Tests..."
  l <- readTList "scan-legal/l"
  scannerlegal <- readTests (addPath "scan-legal" l) True scanner_
  l <- readTList "scan-illegal/l"  
  scannerillegal <- readTests (addPath "scan-illegal" l) False scanner_
  runTestTT scannerlegal
  runTestTT scannerillegal
  

  putStrLn "\nParser Tests..."  
  hiddenparselegals <- readTests (addPath "pars-legal" $ makeLegals 21 31) True parser_
  hiddenparseillegals <- readTests (addPath "pars-illegal" $ makeIllegals 21 40) False parser_
  parsil2 <- readTests (addPath "pars-illegal2" $ makeIllegals 1 6) False parser_
  runTestTT hiddenparselegals
  runTestTT hiddenparseillegals
  runTestTT parsil2

  -- all semchecker and coge tests are parer legal
  hiddenSemLegals <- readTests (addPath "semi-legal" $ makeLegals 1 18) True parser_
  hiddenSemIllegals <- readTests (addPath "semi-illegal" $ makeIllegals 1 52) True parser_
  tests2 <- readTests (addPath "semi-legal2" $ makeLegals 1 11) True parser_
  testsillegal2 <- readTests (addPath "semi-illegal2" $ makeIllegals 1 56) True parser_
  runTestTT hiddenSemLegals
  runTestTT hiddenSemIllegals
  runTestTT tests2
  runTestTT testsillegal2

  cil2 <- readTests (addPath "coge-illegal2" $ makeIllegals 1 4) True parser_
  l <- readTList "coge/l"  
  cc <- readTests (addExt "dcf" $ addPath "coge" l) True parser_
  l <- readTList "coge2/l"  
  cc2 <- readTests (addExt "dcf" $ addPath "coge2" l) True parser_
  l <- readTList "coge3/l"  
  cc3 <- readTests (addExt "dcf" $ addPath "coge3" l) True parser_
  runTestTT cil2
  runTestTT cc
  runTestTT cc2
  runTestTT cc3



  putStrLn "\nSemchecker Tests..."
  hiddenSemLegals <- readTests (addPath "semi-legal" $ makeLegals 1 18) True semchecker_
  hiddenSemIllegals <- readTests (addPath "semi-illegal" $ makeIllegals 1 52) False semchecker_
  tests2 <- readTests (addPath "semi-legal2" $ makeLegals 1 11) True semchecker_
  testsillegal2 <- readTests (addPath "semi-illegal2" $ makeIllegals 1 56) False semchecker_
  runTestTT hiddenSemLegals
  runTestTT hiddenSemIllegals
  runTestTT tests2
  --runTestTT testsillegal2

  cil2 <- readTests (addPath "coge-illegal2" $ makeIllegals 1 4) True semchecker_
  l <- readTList "coge/l"  
  cc <- readTests (addExt "dcf" $ addPath "coge" l) True semchecker_
  l <- readTList "coge2/l"  
  cc2 <- readTests (addExt "dcf" $ addPath "coge2" l) True semchecker_
  l <- readTList "coge3/l"  
  cc3 <- readTests (addExt "dcf" $ addPath "coge3" l) True semchecker_
  runTestTT cil2
  runTestTT cc
  runTestTT cc2
  runTestTT cc3



  putStrLn "\ncodegen Tests..."
  cil2 <- readTestsRuntimeError (addPath "coge-illegal2" $ makeIllegals 1 4) cogen_ coderun
  l <- readTList "coge/l"  
  runTestTT cil2


  cc <- readTestsWithOut (addPath "coge" l) cogen_ coderun
  l <- readTList "coge2/l"  
  cc2 <- readTestsWithOut (addPath "coge2" l) cogen_ coderun
  l <- readTList "coge3/l"  
  cc3 <- readTestsWithOut (addPath "coge3" l) cogen_ coderun

  runTestTT cc
  runTestTT cc2
  runTestTT cc3

