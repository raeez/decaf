module Main where
import Test.HUnit
--import Decaf.ScannerTests
--import Decaf.ParserTests
import Decaf.SemcheckerTests
import Decaf.Util.Prelude


makeFileNames prefix n = map (\x->prefix++(g x)++".dcf") [1..n]
    where g x | x > 9  = show x
              | x <= 9 = "0"++(show x)

makeLegals = makeFileNames "legal-"
makeIllegals = makeFileNames "illegal-"


readTests :: [String] -> Bool -> IO Test
readTests names expected =
    do
      files <- sequence (map readFile names)
      return $TestList.(map (uncurry TestLabel)) $ zip names (map (\cont -> (semchecker_ cont) ~=? expected) files)

main = do
--  putStrLn "Scanner Tests..."
--  runTestTT scannerTests  
--  putStrLn "Parser Tests..."
--  runTestTT parserTests
  putStrLn "Semchecker Tests..."

  hiddenSemLegals <- readTests (makeLegals 19) True
  let illNames = makeFileNames "sem-illegal/illegal-" 52

  hiddenSemIllegals <- readTests illNames False

  runTestTT hiddenSemLegals
  runTestTT hiddenSemIllegals



