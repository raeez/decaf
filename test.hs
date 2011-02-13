module Main
where
import Test.HUnit
import Decaf
import Text.ParserCombinators.Parsec hiding (spaces)

data Report a = Success a
              | Error String
              deriving (Show, Eq)

tests = TestList [
  literalTests,
  identifierTests
  ]

main = do runTestTT tests

--------------------------------------------
-- literalTests
--
parseLiteral :: Parser DecafLiteral
parseLiteral = do
              ws
              literal >>= return

parseLiteralStream :: Parser [DecafLiteral]
parseLiteralStream = parseLiteral `sepBy` separator

readLiterals :: String -> Report [DecafLiteral]
readLiterals input = case parse parseLiteralStream "literal-test-scanner" input of
                      Left err -> Error ("Parser Error!: " ++ show err)
                      Right val -> Success val
literalTests = TestList [
  numbers,
  strings,
  characters,
  comments
  ]

--------------------------------------------
-- identifierTests
--
parseIdentifier :: Parser DecafIdentifier 
parseIdentifier = do
              ws
              identifier >>= return

parseIdentifierStream :: Parser [DecafIdentifier]
parseIdentifierStream = parseIdentifier `sepBy` separator

readIdentifiers :: String -> Report [DecafIdentifier]
readIdentifiers input = case parse parseIdentifierStream "identifier-test-scanner" input of
                          Left err -> Error ("Parser Error!: " ++ show err)
                          Right val -> Success val

identifierTests = TestList [
  variables
  ]

numbers = TestList [
  TestLabel "decimal-0" (readLiterals "0" ~=? Success [(DNumLit . DDec) 0]),
  TestLabel "decimal-100" (readLiterals "100" ~=? Success [(DNumLit . DDec) 100]),
  TestLabel "hexadecimal-0" (readLiterals "0x000" ~=? Success [(DNumLit . DHex) 0]),
  TestLabel "hexadecimal-1" (readLiterals "0x001" ~=? Success [(DNumLit . DHex) 1]),
  TestLabel "hexadecimal-50" (readLiterals "0x32" ~=? Success [(DNumLit . DHex) 50]),
  TestLabel "dec10-hex16" (readLiterals "10\r\r\n 0x10" ~=? Success [(DNumLit . DDec) 10, (DNumLit . DHex) 16]),
  TestLabel "enum 1...10" (readLiterals "1     \t\t\t 2   \n\n\r\n   3 4 5\t6 0x7 0x8//haha, check out this comment ;-) xor'djuggajugga\n0x9//here we go again[0]@!\"!#@$!@#$!@%#$^%$^&%^&*^&*(\"\n0x1000" ~=? Success [(DNumLit . DDec) 1, (DNumLit . DDec) 2, (DNumLit . DDec) 3, (DNumLit . DDec) 4, (DNumLit . DDec) 5, (DNumLit . DDec) 6, (DNumLit . DHex) 7, (DNumLit . DHex) 8, (DNumLit . DHex) 9, (DNumLit . DHex) 4096])
  ]

strings = TestList [
  TestLabel "zero-string" (readLiterals "\"\"" ~=? Success [(DStrLit . DStr) ""]),
  TestLabel "null-string" (readLiterals "\"\0\"" ~=? Success [(DStrLit . DStr) "\0"]),
  TestLabel "escaped-string" (readLiterals "\"a\nbc\"" ~=? Success [(DStrLit . DStr) "a\nbc"]),
  TestLabel "spaced-string" (readLiterals "  \t  \n\"  a   b   c  \"" ~=? Success [(DStrLit . DStr) "  a   b   c  "]),
  TestLabel "mixed-string" (readLiterals "  \"  \\\"heLlo, I aM He\nre to sa\tve yoU!!\0!!\\\"\"" ~=? Success [(DStrLit . DStr) "  \"heLlo, I aM He\nre to sa\tve yoU!!\0!!\""])
  ]

characters = TestList [
  TestLabel "single-char" (readLiterals "'A'" ~=? Success [(DCharLit . DChar) 'A']),
  TestLabel "badbeef-char" (readLiterals "'b' 'a' 'd' 'b' 'e' 'e' 'f'" ~=? Success [(DCharLit . DChar) 'b', (DCharLit . DChar) 'a', (DCharLit . DChar) 'd', (DCharLit . DChar) 'b', (DCharLit . DChar) 'e', (DCharLit . DChar) 'e', (DCharLit . DChar) 'f']),
  TestLabel "char-string-num" (readLiterals "'.' \"B\" 3" ~=? Success [(DCharLit . DChar) '.', (DStrLit . DStr) "B", (DNumLit . DDec) 3])
  ]

comments = TestList [
  TestLabel "basic-comment" (readLiterals "23//test\n24" ~=? Success [(DNumLit . DDec) 23, (DNumLit . DDec) 24]),
  TestLabel "complex-comment" (readLiterals " 4// the cow, he jumped! and this happened 0x2 times, afterwards, giant[3] said \"ho,ho,ho! find for fee fum; @!#$!@#^%$^*(&*^(!@$\"\"\"\n\"test_string\"" ~=? Success [(DNumLit . DDec) 4, (DStrLit . DStr) "test_string"])
  ]

variables = TestList [
  TestLabel "simple-vars" (readIdentifiers "a Db cd e_D fgh_iJKL _g0g0_gadg3t" ~=? Success [DecafID "a",DecafID "Db",DecafID "cd",DecafID "e_D",DecafID "fgh_iJKL",DecafID "_g0g0_gadg3t"]),
  TestLabel "non-keywords" (readIdentifiers "iF If BooLeaN booLean breaK foR For VoiD InTTruE truE inT Class CLASS CONTINuE BreAK fi callouT elsE reTurN" ~=? Success [DecafID "iF",DecafID "If",DecafID "BooLeaN",DecafID "booLean",DecafID "breaK",DecafID "foR",DecafID "For",DecafID "VoiD",DecafID "InTTruE",DecafID "truE",DecafID "inT",DecafID "Class",DecafID "CLASS",DecafID "CONTINuE",DecafID "BreAK",DecafID "fi",DecafID "callouT",DecafID "elsE",DecafID "reTurN"]),
  TestLabel "keywords" (readIdentifiers "break break callout boolean if if int return true void void false else class continue for" ~=? Success [DecafKeyword "break",DecafKeyword "break",DecafKeyword "callout",DecafKeyword "boolean",DecafKeyword "if",DecafKeyword "if",DecafKeyword "int",DecafKeyword "return",DecafKeyword "true",DecafKeyword "void",DecafKeyword "void",DecafKeyword "false",DecafKeyword "else",DecafKeyword "class",DecafKeyword "continue",DecafKeyword "for"]),
  TestLabel "mixed" (readIdentifiers "breakbreak break callout_ boolean if if int return true void void false eLse clasS continue f0r" ~=? Success [DecafID "breakbreak",DecafKeyword "break",DecafID "callout_",DecafKeyword "boolean",DecafKeyword "if",DecafKeyword "if",DecafKeyword "int",DecafKeyword "return",DecafKeyword "true",DecafKeyword "void",DecafKeyword "void",DecafKeyword "false",DecafID "eLse",DecafID "clasS",DecafKeyword "continue",DecafID "f0r"]),
  TestLabel "invalid-!" (readIdentifiers "break!" ~=? Error ""),
  TestLabel "invalid-3" (readIdentifiers "3l3t3" ~=? Error "")
  ]


keywords = [
  "boolean",
  "break",
  "callout",
  "class",
  "continue",
  "else",
  "false",
  "for",
  "if",
  "int",
  "return",
  "true",
  "void"
  ]
