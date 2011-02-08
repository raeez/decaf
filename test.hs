module Main
where
import Test.HUnit
import Decaf
import Text.ParserCombinators.Parsec hiding (spaces)

main = do runTestTT tests

readExpr :: String -> DecafExpr
readExpr input = case parse parseExpr "decaf" input of
                  Left err -> ParseError $ "No Match: " ++ show err
                  Right val -> val

tests = TestList [
  TestLabel "whitespace" testWhitespace,
  TestLabel "atom" testAtom,
  TestLabel "string" testString,
  TestLabel "number" testNumber,
  TestLabel "list" testList
  ]

testWhitespace = TestList [
  readExpr "   %" ~=? Atom "%",
  readExpr " \n%" ~=? Atom "%",
  readExpr "\t\n   ^" ~=? Atom "^",
  readExpr "\t\n   *\n*" ~=? Atom "*",
  readExpr " :" ~=? Atom ":"
  ]

testAtom = TestList [
  readExpr "$" ~=? Atom "$",
  readExpr "!" ~=? Atom "!",
  readExpr "!!" ~=? Atom "!!",
  readExpr "ƒ" ~=? Atom "\402"
  ]

testString = TestList [
  readExpr "\"one two three\"" ~=? String "one two three",
  readExpr "\"xyz\n\nddd\"" ~=? String "xyz\n\nddd",
  readExpr "\"a\tb\nc\"" ~=? String "a\tb\nc"
  ]

testNumber = TestList [
  readExpr "55" ~=? Number 55,
  readExpr "103" ~=? Number 103
  ]
testList = TestList [
  readExpr "(a test)" ~=? List [Atom "a", Atom "test"],
  readExpr "(a (nested) test)" ~=? List [Atom "a", List [Atom "nested"], Atom "test"],
  readExpr "(a (dotted . list) test)" ~=? List [Atom "a", DottedList [Atom "dotted"] (Atom "list"), Atom "test"],
  readExpr "(a '(quoted (dotted . list)) test)" ~=? List [Atom "a",List [Atom "quote",List [Atom "quoted",DottedList [Atom "dotted"] (Atom "list")]],Atom "test"],
  readExpr "(a '(imbalanced parens)" ~=? ParseError "No Match: \"lisp\" (line 1, column 24):\nunexpected end of input\nexpecting space or \")\""
  ]
