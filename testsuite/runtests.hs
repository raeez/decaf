module Main where
import Test.HUnit
import Decaf.ScannerTests
import Decaf.ParserTests
import Decaf.SemcheckerTests

main = do
  putStrLn "Scanner Tests..."
  runTestTT scannerTests  
  putStrLn "Parser Tests..."
  runTestTT parserTests
  putStrLn "Semchecker Tests..."
  runTestTT semcheckerTests

