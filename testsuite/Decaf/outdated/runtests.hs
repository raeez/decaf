module Main
where
import Test.HUnit
import ScannerParserTests
import SemanticsTests
import Text.ParserCombinators.Parsec hiding (spaces)

main = do 
  runScannerParserTests
  runSemanticsTests
  




