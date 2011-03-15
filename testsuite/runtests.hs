module Main where
import Test.HUnit
--import Decaf.ScannerTests
--import Decaf.ParserTests
--import Decaf.SemcheckerTests
import Decaf.Checker
import Decaf.Tokens
import Decaf.Scanner
import Decaf.Parser
import Decaf.Util.Report
import Decaf.Util.Prelude


-------------------------------------
-- program to be tested
scanner_ :: String -> Bool
scanner_ i = if numLexErrorsIn (scanner i) > 0 then False else True

parser_ :: String -> Bool
parser_ i = case ps program i of
        RError err -> False
        RSuccess val -> True

semchecker_ :: String -> Bool                      
semchecker_  = checker 

-- cogen
cogen_ :: String -> [String]
cogen_ = ["\t.global main", "main:", "\tret"]

-- cogen legal, returns true if running without runtime error
cogenl_ :: String -> Bool
cogenl_ = True

-- cogen content, return program output
cogenc_ :: String -> String
cogenc_ = ""



------------------------------------
-- test programs

makeFileNames prefix s e = map (\x->prefix++(g x)++".dcf") [s..e]
    where g x | x > 9  = show x
              | x <= 9 = "0"++(show x)

makeLegals = makeFileNames "legal-"
makeIllegals = makeFileNames "illegal-"


readTests :: [String] -> Bool -> (String -> Bool) -> IO Test
readTests names expected func =
    do
      files <- sequence (map readFile names)
      return $TestList.(map (uncurry TestLabel)) $ zip names (map (\cont -> expected ~=? (func cont)) files)
      
      
-- read tests and their .out file      : Finish me 
readTestsWithOut :: [String] -> Bool -> (String -> Bool) -> IO Test
readTestsWithOut names expected func =
    do
      files <- sequence (map readFile $ map (\x -> x++".dcf") names)
      outs <- sequence (map readFile $ map (\x -> x++".out") names)
      let tests = map (\x -> snd x ~=? fst x) $ zip files outs 
      return $TestList.(map (uncurry TestLabel)) $ zip names tests
      
      

-- | read test from a list
readTList :: String -> IO [String]
readTList ln = readFile ln >>= return . lines


addPath :: String -> [String] -> [String]
addPath path = map (\x -> path++"/"++x)


main = do
  putStrLn "Scanner Tests..."
  l <- readTList "scan-legal/l"
  scannerlegal <- readTests (addPath "scan-legal" l) True scanner_
  l <- readTList "scan-illegal/l"  
  scannerillegal <- readTests (addPath "scan-illegal" l) False scanner_
  runTestTT scannerlegal
  runTestTT scannerillegal
  

  putStrLn "Parser Tests..."  
  hiddenparselegals <- readTests (addPath "pars-legal" $ makeLegals 21 31) True parser_
  hiddenparseillegals <- readTests (addPath "pars-illegal" $ makeIllegals 21 40) False parser_
  runTestTT hiddenparselegals
  runTestTT hiddenparseillegals


  putStrLn "Semchecker Tests..."
  hiddenSemLegals <- readTests (addPath "semi-legal" $ makeLegals 1 18) True semchecker_
  hiddenSemIllegals <- readTests (addPath "semi-illegal" $ makeIllegals 1 52) False semchecker_
  tests2 <- readTests (addPath "semi-legal2" $ makeLegals 1 11) True semchecker_
  testsillegal2 <- readTests (addPath "semi-illegal2" $ makeIllegals 1 62) False semchecker_
  runTestTT hiddenSemLegals
  runTestTT hiddenSemIllegals
  runTestTT tests2
  runTestTT testsillegal2


  putStrLn "codegen Tests..."
  cl2 <- readTests (addPath "semi-legal2" $ makeLegals 1 3) True cogenl_
  cil2 <- readTests (addPath "semi-illegal2" $ makeIllegals 1 4) False cogenl_
  l <- readTList "coge/l"  
  cc <- readTestsWithOut (addPath "coge" l) cogenc_  
  l <- readTList "coge2/l"  
  cc2 <- readTestsWithOut (addPath "coge2" l) cogenc_  
  l <- readTList "coge3/l"  
  cc2 <- readTestsWithOut (addPath "coge3" l) cogenc_  
  runTestTT cl2
  runTestTT cil2
  runTestTT cc
  runTestTT cc2
  runTestTT cc3

