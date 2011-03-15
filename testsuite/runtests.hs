module Main where
import Test.HUnit
--import Decaf.ScannerTests
--import Decaf.ParserTests
--import Decaf.SemcheckerTests
import Decaf.Checker
import Decaf.Scanner
import Decaf.Parser
import Decaf.Util.Report


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

-- cogen
cogen_ :: String -> [String]
cogen_ _ = ["\t.global main", "main:", "\tret"]

-- cogen legal, returns true if running without runtime error
cogenl_ :: String -> Bool
cogenl_ _ = True

-- cogen content, return program output
cogenc_ :: String -> String
cogenc_ _ = ""



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
readTestsWithOut :: [String] -> (String -> String) -> IO Test
readTestsWithOut names func =
    do
      files <- sequence (map readFile $ map (\x -> x++".dcf") names)
      outs <- sequence (map readFile $ map (\x -> x++".out") names)
      let tests = map (\x -> snd x ~=? (func $ fst x)) $ zip files outs   -- fix Func, func need to go through gcc
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
  runTestTT hiddenparselegals
  runTestTT hiddenparseillegals

  -- all semchecker and coge tests are parer legal
  hiddenSemLegals <- readTests (addPath "semi-legal" $ makeLegals 1 18) True parser_
  hiddenSemIllegals <- readTests (addPath "semi-illegal" $ makeIllegals 1 52) True parser_
  tests2 <- readTests (addPath "semi-legal2" $ makeLegals 1 11) True parser_
  testsillegal2 <- readTests (addPath "semi-illegal2" $ makeIllegals 1 62) True parser_
  runTestTT hiddenSemLegals
  runTestTT hiddenSemIllegals
  runTestTT tests2
  runTestTT testsillegal2

  cl2 <- readTests (addPath "semi-legal2" $ makeLegals 1 3) True parser_
  cil2 <- readTests (addPath "semi-illegal2" $ makeIllegals 1 4) True parser_
  l <- readTList "coge/l"  
  cc <- readTests (addExt "dcf" $ addPath "coge" l) True parser_
  l <- readTList "coge2/l"  
  cc2 <- readTests (addExt "dcf" $ addPath "coge2" l) True parser_
  l <- readTList "coge3/l"  
  cc3 <- readTests (addExt "dcf" $ addPath "coge3" l) True parser_
  runTestTT cl2
  runTestTT cil2
  runTestTT cc
  runTestTT cc2
  runTestTT cc3



  putStrLn "\nSemchecker Tests..."
  hiddenSemLegals <- readTests (addPath "semi-legal" $ makeLegals 1 18) True semchecker_
  hiddenSemIllegals <- readTests (addPath "semi-illegal" $ makeIllegals 1 52) False semchecker_
  tests2 <- readTests (addPath "semi-legal2" $ makeLegals 1 11) True semchecker_
  testsillegal2 <- readTests (addPath "semi-illegal2" $ makeIllegals 1 62) False semchecker_
  runTestTT hiddenSemLegals
  runTestTT hiddenSemIllegals
  runTestTT tests2
  runTestTT testsillegal2

  cl2 <- readTests (addPath "semi-legal2" $ makeLegals 1 3) True semchecker_
  cil2 <- readTests (addPath "semi-illegal2" $ makeIllegals 1 4) True semchecker_
  l <- readTList "coge/l"  
  cc <- readTests (addExt "dcf" $ addPath "coge" l) True semchecker_
  l <- readTList "coge2/l"  
  cc2 <- readTests (addExt "dcf" $ addPath "coge2" l) True semchecker_
  l <- readTList "coge3/l"  
  cc3 <- readTests (addExt "dcf" $ addPath "coge3" l) True semchecker_
  runTestTT cl2
  runTestTT cil2
  runTestTT cc
  runTestTT cc2
  runTestTT cc3





  putStrLn "\ncodegen Tests..."
  cl2 <- readTests (addPath "semi-legal2" $ makeLegals 1 3) True cogenl_
  cil2 <- readTests (addPath "semi-illegal2" $ makeIllegals 1 4) False cogenl_
  l <- readTList "coge/l"  
  cc <- readTestsWithOut (addPath "coge" l) cogenc_  
  l <- readTList "coge2/l"  
  cc2 <- readTestsWithOut (addPath "coge2" l) cogenc_  
  l <- readTList "coge3/l"  
  cc3 <- readTestsWithOut (addPath "coge3" l) cogenc_  
  runTestTT cl2
  runTestTT cil2
  runTestTT cc
  runTestTT cc2
  runTestTT cc3

