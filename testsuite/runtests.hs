module Main where
import Test.HUnit
import Decaf.ScannerTests
import Decaf.ParserTests
import Decaf.SemcheckerTests

main = do
  runTestTT scannerTests  
  runTestTT parserTests
  runTestTT semcheckerTests

