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
  runTestTT hiddenSemLegals
  runTestTT hiddenSemIllegals



