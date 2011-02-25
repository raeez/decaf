module Main
where
import Test.HUnit
import Scanner
import Decaf
import Text.ParserCombinators.Parsec hiding (spaces)

data Report a = Success a
              | Error String
              deriving (Show, Eq)

tests = TestList [
  literalTests,
  identifierTests,
  expressionTests
  ]

main = do runTestTT tests

--------------------------------------------
-- scannerTests
--
scanTokens :: Parser [DecafLiteral]
scanTokens = token `sepEndBy` separator
             
separator = ws       -- don't know if it is right -J 

readTokens :: String -> Report [DecafLiteral]
readTokens input = case parse scanTokens "literal-test-scanner" input of
                      Left err -> Error ("Parser Error!: " ++ show err)
                      Right val -> Success val
{-
literalTests = TestList [
  numbers,
  bools,
  strings,
  characters,
  comments
  ]
-- 2nd copy -J
-}

--------------------------------------------
-- literalTests
--
parseLiteral :: Parser DecafLiteral
parseLiteral = do
              ws
              literal >>= return

parseLiteralStream :: Parser [DecafLiteral]
parseLiteralStream = parseLiteral `sepEndBy` separator

readLiterals :: String -> Report [DecafLiteral]
readLiterals input = case parse parseLiteralStream "literal-test-scanner" input of
                      Left err -> Error ("Parser Error!: " ++ show err)
                      Right val -> Success val
literalTests = TestList [
  numbers,
  bools,
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
  ids
  ]

--------------------------------------------
-- expressionTests
--
parseExpressionStream :: Parser [Integer]
parseExpressionStream = expr `sepBy` separator

readExpressions :: String -> Report [Integer]
readExpressions input = case parse parseExpressionStream "expression-test-scanner" input of
                          Left err -> Error ("Parser Error!: " ++ show err)
                          Right val -> Success val

expressionTests = TestList [
  arithmeticExpressions
  ]

--
--
-- TESTS
--
--
numbers = TestList [
  TestLabel "decimal-0" (readLiterals "0" ~=? Success [(DNumLit . DDec) 0]),
  TestLabel "decimal-100" (readLiterals "100" ~=? Success [(DNumLit . DDec) 100]),
  TestLabel "hexadecimal-0" (readLiterals "0x000" ~=? Success [(DNumLit . DHex) 0]),
  TestLabel "hexadecimal-1" (readLiterals "0x001" ~=? Success [(DNumLit . DHex) 1]),
  TestLabel "hexadecimal-50" (readLiterals "0x32" ~=? Success [(DNumLit . DHex) 50]),
  TestLabel "dec10-hex16" (readLiterals "10\r\r\n 0x10" ~=? Success [(DNumLit . DDec) 10, (DNumLit . DHex) 16]),
  TestLabel "enum 1...10" (readLiterals "1     \t\t\t 2   \n\n\r\n   3 4 5\t6 0x7 0x8//haha, check out this comment ;-) xor'djuggajugga\n0x9//here we go again[0]@!\"!#@$!@#$!@%#$^%$^&%^&*^&*(\"\n0x1000" ~=? Success [(DNumLit . DDec) 1, (DNumLit . DDec) 2, (DNumLit . DDec) 3, (DNumLit . DDec) 4, (DNumLit . DDec) 5, (DNumLit . DDec) 6, (DNumLit . DHex) 7, (DNumLit . DHex) 8, (DNumLit . DHex) 9, (DNumLit . DHex) 4096])
  ]

bools = TestList [
  TestLabel "simple-bools" (readLiterals "true false true" ~=? Success[DBoolLit DTrue, DBoolLit DFalse, DBoolLit DTrue])
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
  TestLabel "char-string-num" (readLiterals "'.' \"B\" 3" ~=? Success [(DCharLit . DChar) '.', (DStrLit . DStr) "B", (DNumLit . DDec) 3]),
  TestLabel "chained-characters" (readLiterals "'.''A''B'" ~=? Success [])
  ]

comments = TestList [
  TestLabel "basic-comment" (readLiterals "23//test\n24" ~=? Success [(DNumLit . DDec) 23, (DNumLit . DDec) 24]),
  TestLabel "complex-comment" (readLiterals " 4// the cow, he jumped! and this happened 0x2 times, afterwards, giant[3] said \"ho,ho,ho! find for fee fum; @!#$!@#^%$^*(&*^(!@$\"\"\"\n\"test_string\"" ~=? Success [(DNumLit . DDec) 4, (DStrLit . DStr) "test_string"])
  ]

ids = TestList [
  TestLabel "simple-vars" (readIdentifiers "a Db cd e_D fgh_iJKL _g0g0_gadg3t" ~=? Success [DecafID "a",DecafID "Db",DecafID "cd",DecafID "e_D",DecafID "fgh_iJKL",DecafID "_g0g0_gadg3t"]),
  TestLabel "non-keywords" (readIdentifiers "iF If BooLeaN booLean breaK foR For VoiD InTTruE truE inT Class CLASS CONTINuE BreAK fi callouT elsE reTurN" ~=? Success [DecafID "iF",DecafID "If",DecafID "BooLeaN",DecafID "booLean",DecafID "breaK",DecafID "foR",DecafID "For",DecafID "VoiD",DecafID "InTTruE",DecafID "truE",DecafID "inT",DecafID "Class",DecafID "CLASS",DecafID "CONTINuE",DecafID "BreAK",DecafID "fi",DecafID "callouT",DecafID "elsE",DecafID "reTurN"]),
  TestLabel "keywords" (readIdentifiers "break break callout boolean if if int return true void void false else class continue for" ~=? Success [DecafKeyword "break",DecafKeyword "break",DecafKeyword "callout",DecafKeyword "boolean",DecafKeyword "if",DecafKeyword "if",DecafKeyword "int",DecafKeyword "return",DecafKeyword "true",DecafKeyword "void",DecafKeyword "void",DecafKeyword "false",DecafKeyword "else",DecafKeyword "class",DecafKeyword "continue",DecafKeyword "for"]),
  TestLabel "mixed" (readIdentifiers "breakbreak break callout_ boolean if if int return true void void false eLse clasS continue f0r" ~=? Success [DecafID "breakbreak",DecafKeyword "break",DecafID "callout_",DecafKeyword "boolean",DecafKeyword "if",DecafKeyword "if",DecafKeyword "int",DecafKeyword "return",DecafKeyword "true",DecafKeyword "void",DecafKeyword "void",DecafKeyword "false",DecafID "eLse",DecafID "clasS",DecafKeyword "continue",DecafID "f0r"]),
  TestLabel "invalid-!" (readIdentifiers "break!" ~=? Error ""),
  TestLabel "invalid-3" (readIdentifiers "3l3t3" ~=? Error "")
  ]

arithmeticExpressions = TestList [
  TestLabel "simple-expressions" (readExpressions "3+2*2 3*5+6 21/3-2" ~=? Success [7,21,5]),
  TestLabel "paren-expressions" (readExpressions "(3+6*(2-11)) (21+21/21)/22" ~=? Success [-51, 1])
  ]



--------------------------------------------
-- Semantics Checking Test
--

{-


semanticCheck :: String -> Report Bool   -- right now just pass or fail
semanticCheck input = False
  --case parse parseExpressionStream "decaf-semantic-checker" input of
  --                        Left err -> Error ("Parser Error!: " ++ show err)
  --                        Right val -> Success val




semanticTests = TestList [
  smt1_no_decl_twice,
  
  
  
  ]
                
                
                
-- 1. No identifier is declared twice in the same scope.
smr1_no_decl_twice = TestList [
  TestLabel "case-given-by-TA" (semanticCheck "class Program {\n void main() { \n int x; boolean x;  // identifier declared twice \n }\n}"
                                == False)
  ]


-- 2. No identifier is used before it is declared.
smr2_use_after_decl = TestList [
  
  
  ]


-}

{-





3. The program contains a definition for a method called main that has no parameters (note
that since execution starts at method main, any methods defined after main will never be
executed).
4. The <int literal> in an array declaration must be greater than 0.
5. The number and types of arguments in a method call must be the same as the number and
types of the formals, i.e., the signatures must be identical.
6. If a method call is used as an expression, the method must return a result.
7. A return statement must not have a return value unless it appears in the body of a method
that is declared to return a value.
8. The expression in a return statement must have the same type as the declared result type
of the enclosing method definition.
9. An <id> used as a <location> must name a declared local/global variable or formal parameter.
10. For all locations of the form <id>[<expr>]
(a) <id> must be an array variable, and
(b) the type of <expr> must be int.
11. The <expr> in an if statement must have type boolean.
12. The operands of <arith op>s and hrel opis must have type int.
13. The operands of <eq op>s must have the same type, either int or boolean.
14. The operands of <cond op>s and the operand of logical not (!) must have type boolean.
15. The <location> and the <expr> in an assignment, <location> = <expr>, must have the same type.
16. The <location> and the <expr> in an incrementing/decrementing assignment, <location> += <expr>
and <location> -= <expr>, must be of type int.
17. The initial <expr> and the ending <expr> of for must have type int.
18. All break and continue statements must be contained within the body of a for.










-}
