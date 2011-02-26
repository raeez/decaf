module ScannerParserTests
where
import Test.HUnit
import Decaf.AST
import Decaf.Scanner
import Decaf.Parser
import Decaf.Tokens
import Text.ParserCombinators.Parsec hiding (spaces)



runScannerParserTests = do runTestTT scannerParserTestLists
  

scannerParserTestLists = TestList [
  scannerlTests,
  parserTests
  ]


------------------------------------------
-- scannerTests
--

scanTokens_ :: Parser [Token]
scanTokens_ = singleToken `sepEndBy` separator
              where separator = ws <|> end


scanTokens :: String -> Report [DecafToken]
scanTokens input = case parse scanTokens_ "test-scanner" input of
                      Left err -> RError ("Parser Error!: " ++ show err)
                      Right val -> RSuccess (map dToken val) 


scannerTests = TestList [
  numbers,
  bools,
  strings,
  characters,
  comments,
  ids
  ]

numbers = TestList [
  TestLabel "decimal-0" (scanTokens "0" ~=? RSuccess [DecLit 0]),
  TestLabel "decimal-100" (scanTokens "100" ~=? RSuccess [DecLit 100]),
  TestLabel "hexadecimal-0" (scanTokens "0x000" ~=? RSuccess [HexLit 0]),
  TestLabel "hexadecimal-1" (scanTokens "0x001" ~=? RSuccess [HexLit 1]),
  TestLabel "hexadecimal-50" (scanTokens "0x32" ~=? RSuccess [HexLit 50]),
  TestLabel "dec10-hex16" (scanTokens "10\r\r\n 0x10" ~=? RSuccess [DecLit 10, HexLit 16]),
  TestLabel "enum 1...10" (scanTokens "1     \t\t\t 2   \n\n\r\n   3 4 5\t6 0x7 0x8//haha, check out this comment ;-) xor'djuggajugga\n0x9//here we go again[0]@!\"!#@$!@#$!@%#$^%$^&%^&*^&*(\"\n0x1000" ~=? RSuccess [DecLit 1, DecLit 2, DecLit 3, DecLit 4, DecLit 5, DecLit 6, HexLit 7, HexLit 8, HexLit 9, HexLit 4096])
  ]

bools = TestList [
  TestLabel "simple-bools" (scanTokens "true false true" ~=? RSuccess[BoolLit True, BoolLit False, BoolLit True])
  ]

strings = TestList [
  TestLabel "zero-string" (scanTokens "\"\"" ~=? RSuccess [StrLit ""]),
  TestLabel "null-string" (scanTokens "\"\0\"" ~=? RSuccess [StrLit "\0"]),
  TestLabel "escaped-string" (scanTokens "\"a\nbc\"" ~=? RSuccess [StrLit "a\nbc"]),
  TestLabel "spaced-string" (scanTokens "  \t  \n\"  a   b   c  \"" ~=? RSuccess [StrLit "  a   b   c  "]),
  TestLabel "mixed-string" (scanTokens "  \"  \\\"heLlo, I aM He\nre to sa\tve yoU!!\0!!\\\"\"" ~=? RSuccess [StrLit "  \"heLlo, I aM He\nre to sa\tve yoU!!\0!!\""])
  ]

characters = TestList [
  TestLabel "single-char" (scanTokens "'A'" ~=? RSuccess [CharLit 'A']),
  TestLabel "badbeef-char" (scanTokens "'b' 'a' 'd' 'b' 'e' 'e' 'f'" ~=? RSuccess [CharLit 'b', CharLit 'a', CharLit 'd', CharLit 'b', CharLit 'e', CharLit 'e', CharLit 'f']),
  TestLabel "char-string-num" (scanTokens "'.' \"B\" 3" ~=? RSuccess [CharLit '.', StrLit "B", DecLit 3]),
  TestLabel "chained-characters" (scanTokens "'.''A''B'" ~=? RSuccess [])
  ]

comments = TestList [
  TestLabel "basic-comment" (scanTokens "23//test\n24" ~=? RSuccess [DecLit 23, DecLit 24]),
  TestLabel "complex-comment" (scanTokens " 4// the cow, he jumped! and this happened 0x2 times, afterwards, giant[3] said \"ho,ho,ho! find for fee fum; @!#$!@#^%$^*(&*^(!@$\"\"\"\n\"test_string\"" ~=? RSuccess [DecLit 4, StrLit "test_string"])
  ]


ids = TestList [
  TestLabel "simple-vars" (scanTokenss "a Db cd e_D fgh_iJKL _g0g0_gadg3t" ~=? RSuccess [Identf "a",Identf "Db",Identf "cd",Identf "e_D",Identf "fgh_iJKL",Identf "_g0g0_gadg3t"]),
  TestLabel "non-keywords" (scanTokenss "iF If BooLeaN booLean breaK foR For VoiD InTTruE truE inT Class CLASS CONTINuE BreAK fi callouT elsE reTurN" ~=? RSuccess [Identf "iF",Identf "If",Identf "BooLeaN",Identf "booLean",Identf "breaK",Identf "foR",Identf "For",Identf "VoiD",Identf "InTTruE",Identf "truE",Identf "inT",Identf "Class",Identf "CLASS",Identf "CONTINuE",Identf "BreAK",Identf "fi",Identf "callouT",Identf "elsE",Identf "reTurN"]),
  TestLabel "keywords" (scanTokenss "break break callout boolean if if int return true void void false else class continue for" ~=? RSuccess [Reserv "break",Reserv "break",Reserv "callout",Reserv "boolean",Reserv "if",Reserv "if",Reserv "int",Reserv "return",Reserv "true",Reserv "void",Reserv "void",Reserv "false",Reserv "else",Reserv "class",Reserv "continue",Reserv "for"]),
  TestLabel "mixed" (scanTokenss "breakbreak break callout_ boolean if if int return true void void false eLse clasS continue f0r" ~=? RSuccess [Identf "breakbreak",Reserv "break",Identf "callout_",Reserv "boolean",Reserv "if",Reserv "if",Reserv "int",Reserv "return",Reserv "true",Reserv "void",Reserv "void",Reserv "false",Identf "eLse",Identf "clasS",Reserv "continue",Identf "f0r"]),
  TestLabel "invalid-!" (scanTokenss "break!" ~=? RError ""),
  TestLabel "invalid-3" (scanTokenss "3l3t3" ~=? RError "")
  ]






--------------------------------------------
-- parser test
--


parseExpressionStream :: Parser [Integer]
parseExpressionStream = expr `sepBy` separator

readExpressions :: String -> Report [Integer]
readExpressions input = case parse parseExpressionStream "expression-test-scanner" input of
                          Left err -> RError ("Parser Error!: " ++ show err)
                          Right val -> RSuccess val

parseProgrm :: String -> 


parserTests = TestList [
  arithmeticExpressions
  ]


arithmeticExpressions = TestList [
  TestLabel "simple-expressions" (readExpressions "3+2*2 3*5+6 21/3-2" ~=? RSuccess [7,21,5]),
  TestLabel "paren-expressions" (readExpressions "(3+6*(2-11)) (21+21/21)/22" ~=? RSuccess [-51, 1])
  ]








{-
scanTokens :: Parser [DecafLiteral]
scanTokens = token `sepEndBy` ws        -- don't know if it is right -J 

readTokens :: String -> Report [DecafLiteral]
readTokens input = case parse scanTokens "literal-test-scanner" input of
                      Left err -> RError ("Parser Error!: " ++ show err)
                      Right val -> RSuccess val

--------------------------------------------
-- scanner tests
--

--DecafToken2DecafLiteral :: Scanner.Token -> DecafLiteral
--dTok2DLit d = DDec 0   -- temp -J
  

parseLiteral :: Parser DecafLiteral
parseLiteral = do
              ws
              literal >>= return 

parseLiteralStream :: Parser [DecafLiteral]
parseLiteralStream = parseLiteral `sepEndBy` separator

readLiterals :: String -> Report [DecafLiteral]
readLiterals input = case parse parseLiteralStream "literal-test-scanner" input of
                      Left err -> RError ("Parser Error!: " ++ show err)
                      Right val -> RSuccess val




parseIdentifier :: Parser DecafIdentifier 
parseIdentifier = do
              ws
              identifier >>= return

parseIdentifierStream :: Parser [DecafIdentifier]
parseIdentifierStream = parseIdentifier `sepBy` separator

readIdentifiers :: String -> Report [DecafIdentifier]
readIdentifiers input = case parse parseIdentifierStream "identifier-test-scanner" input of
                          Left err -> RError ("Parser Error!: " ++ show err)
                          Right val -> RSuccess val
-}