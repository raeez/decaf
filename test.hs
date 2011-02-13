module Main
where
import Test.HUnit
import Decaf
import Text.ParserCombinators.Parsec hiding (spaces)

main = do runTestTT tests

--------------------------------------------
-- types
--
typeDecl :: Parser DecafType
typeDecl =  (string "boolean" >> return DBoolean)
        <|> (string "integer" >> return DInteger)
        <|> (string "void" >> return DVoid)

--------------------------------------------
-- Test everything below with these
--

parseLiteral :: Parser DecafLiteral
parseLiteral = do
              ws
              literal >>= return

parseLiteralTokenStream :: Parser [DecafLiteral]
parseLiteralTokenStream = parseToken `sepBy` separator
literalTokenizer :: String -> [DecafLiteral]
literalTokenizer input = case parse parseLiteralTokenStream "test-scanner" input of
                    Left err -> [ParseError $ ParserErrorMessage $ "Parser Error!: " ++ show err]
                    Right val -> val

tests = TestList [
  scannerTests,
  literalTests,
  ]

--------------------------------------------
-- scannerTests
--
scannerTests = TestList [
  numbers,
  strings,
  characters,
  comments,
  types
  ]

numbers = TestList [
  TestLabel "decimal-0" (readTokens "0" ~=? [DNumber 0]),
  TestLabel "decimal-100" (readTokens "100" ~=? [DNumber 100]),
  TestLabel "hexadecimal-0" (readTokens "0x000" ~=? [DNumber 0]),
  TestLabel "hexadecimal-1" (readTokens "0x001" ~=? [DNumber 1]),
  TestLabel "hexadecimal-50" (readTokens "0x32" ~=? [DNumber 50]),
  TestLabel "dec10-hex16" (readTokens "10\r\r\n 0x10" ~=? [DNumber 10, DNumber 16]),
  TestLabel "enum 1...10" (readTokens "1     \t\t\t 2   \n\n\r\n   3 4 5\t6 0x7 0x8//haha, check out this comment ;-) xor'djuggajugga\n0x9//here we go again[0]@!\"!#@$!@#$!@%#$^%$^&%^&*^&*(\"\n0x1000" ~=? [DNumber 1, DNumber 2, DNumber 3, DNumber 4, DNumber 5, DNumber 6, DNumber 7, DNumber 8, DNumber 9, DNumber 4096])
  ]

strings = TestList [
  TestLabel "zero-string" (readTokens "\"\"" ~=? [DString ""]),
  TestLabel "null-string" (readTokens "\"\0\"" ~=? [DString "\0"]),
  TestLabel "escaped-string" (readTokens "\"a\nbc\"" ~=? [DString "a\nbc"]),
  TestLabel "spaced-string" (readTokens "  \t  \n\"  a   b   c  \"" ~=? [DString "  a   b   c  "]),
  TestLabel "mixed-string" (readTokens "  \"  \\\"heLlo, I aM He\nre to sa\tve yoU!!\0!!\\\"\"" ~=? [DString "  \"heLlo, I aM He\nre to sa\tve yoU!!\0!!\""])
  ]

characters = TestList [
  TestLabel "single-char" (readTokens "'A'" ~=? [DChar 'A']),
  TestLabel "badbeef-char" (readTokens "'b' 'a' 'd' 'b' 'e' 'e' 'f'" ~=? [DChar 'b', DChar 'a', DChar 'd', DChar 'b', DChar 'e', DChar 'e', DChar 'f']),
  TestLabel "char-string-num" (readTokens "'.' \"B\" 3" ~=? [DChar 'a', DString "B", DNumber 3])
  ]

comments = TestList [
  TestLabel "basic-comment" (readTokens "23//test\n24" ~=? [DNumber 23, DNumber 24]),
  TestLabel "complex-comment" (readTokens " 4// the cow, he jumped! and this happened 0x2 times, afterwards, giant[3] said \"ho,ho,ho! find for fee fum; @!#$!@#^%$^*(&*^(!@$\"\"\"\n\"test_string\"" ~=? [DNumber 4, DString "test_string"])
  ]

types = TestList [
  TestLabel "void" (readTokens "void void void" ~=? [tVoid, tVoid, tVoid]

--------------------------------------------
-- scannerTests
--
literalTests = [

]
