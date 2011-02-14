module Main
where
import Test.HUnit
import Scanner
import Text.ParserCombinators.Parsec
import Text.Parsec.Pos

data Report a = Success a -- extract the bit we want out of the tuple
              | Error String
              deriving (Show, Eq)

getReport :: Report a -> a
getReport (Success a) = a

rep :: String -> [DecafToken]
rep s = map snd (getReport $ readTokens s)

--tests = TestList [
  --numbers,
  --bools,
  --strings,
  --characters,
  --comments,
  --ids,
  --expressions
  --]

--main = do runTestTT tests

readTokens :: String -> Report [Token]
readTokens input = case parse tokenStream "test-scanner" input of
                          Left err -> Error ("Parser Error!: " ++ show err)
                          Right val -> Success val

--numbers = TestList [
--  TestLabel "decimal-0" (readTokens "0" ~=? Success [
--                        ("test-scanner" (line 1, column 1),DecLit "0")]),
--
--  TestLabel "decimal-100" (readTokens "100" ~=? Success [
--                        ("test-scanner" (line 1, column 1),DecLit "100")]),
--
--  TestLabel "hexadecimal-0" (readTokens "0x000" ~=? Success [
--                        ("test-scanner" (line 1, column 3),HexLit "001")]),
--
--  TestLabel "hexadecimal-1" (readTokens "0x001" ~=? Success [
--                        ("test-scanner" (line 1, column 3),HexLit "32")]),
--
--  TestLabel "hexadecimal-50" (readTokens "0x32" ~=? Success [
--                        ("test-scanner" (line 1, column 1),DecLit "10"),
--                        ("test-scanner" (line 2, column 4),HexLit "10")]),
--
--  TestLabel "dec10-hex16" (readTokens "10\r\r\n 0x10" ~=? Success [
--                        ("test-scanner" (line 1, column 1),DecLit "1"),
--                        ("test-scanner" (line 1, column 26),DecLit "2"),
--                        ("test-scanner" (line 4, column 4),DecLit "3"),
--                        ("test-scanner" (line 4, column 6),DecLit "4"),
--                        ("test-scanner" (line 4, column 8),DecLit "5"),
--                        ("test-scanner" (line 4, column 17),DecLit "6"),
--                        ("test-scanner" (line 4, column 21),HexLit "7"),
--                        ("test-scanner" (line 4, column 25),HexLit "8"),
--                        ("test-scanner" (line 5, column 3),HexLit "9"),
--                        ("test-scanner" (line 6, column 3),HexLit "1000")]),
--
--  TestLabel "enum 1...10" (readTokens "1     \t\t\t 2   \n\n\r\n   3 4 5\t6 0x7 0x8//haha, check out this comment ;-) xor'djuggajugga\n0x9//here we go again[0]@!\"!#@$!@#$!@%#$^%$^&%^&*^&*(\"\n0x1000" ~=? Success [])
--  ]
--
--bools = TestList [
--  TestLabel "simple-bools" (readTokens "true false true" ~=? Success [
--                        ("test-scanner" (line 1, column 1),BoolLit True),
--                        ("test-scanner" (line 1, column 6),BoolLit False),
--                        ("test-scanner" (line 1, column 12),BoolLit True)])
--  ]
--
--strings = TestList [
--  TestLabel "zero-string" (readTokens "\"\"" ~=? Success [
--                          ("test-scanner" (line 1, column 2),StrLit "")]),
--
--  TestLabel "null-string" (readTokens "\"\0\"" ~=? Success [
--                          ("test-scanner" (line 1, column 2),StrLit "\NUL")]),
--
--  TestLabel "escaped-string" (readTokens "\"a\nbc\"" ~=? Success [
--                          ("test-scanner" (line 1, column 2),StrLit "a\nbc")]),
--
--  TestLabel "spaced-string" (readTokens "  \t  \n\"  a   b   c  \"" ~=? Success []),
--
--  TestLabel "mixed-string" (readTokens "  \"  \\\"heLlo, I aM He\nre to sa\tve yoU!!\0!!\\\"\"" ~=? Success [])
--  ]
--
--characters = TestList [
--  TestLabel "single-char" (readTokens "'A'" ~=? Success [
--                          ("test-scanner" (line 1, column 4),CharLit 'A')]),
--
--  TestLabel "badbeef-char" (readTokens "'b' 'a' 'd' 'b' 'e' 'e' 'f' \n'2'" ~=? Success [
--                          ("test-scanner" (line 1, column 1),CharLit 'b'),
--                          ("test-scanner" (line 1, column 5),CharLit 'a'),
--                          ("test-scanner" (line 1, column 9),CharLit 'd'),
--                          ("test-scanner" (line 1, column 13),CharLit 'b'),
--                          ("test-scanner" (line 1, column 17),CharLit 'e'),
--                          ("test-scanner" (line 1, column 21),CharLit 'e'),
--                          ("test-scanner" (line 1, column 25),CharLit 'f'),
--                          ("test-scanner" (line 2, column 1),CharLit '2')]),
--
--  TestLabel "char-string-num" (readTokens "'.' \"B\" 3" ~=? Success [
--                          ("test-scanner" (line 1, column 4),CharLit '.'),
--                          ("test-scanner" (line 1, column 6),StrLit "B"),
--                          ("test-scanner" (line 1, column 9),DecLit "3")]),
--
--  TestLabel "chained-characters" (readTokens "'.''A''B'" ~=? Success [])
--  ]
--
--comments = TestList [
--  TestLabel "basic-comment" (readTokens "23//test\n24" ~=? Success [
--                          ("test-scanner" (line 1, column 1),DecLit "23"),
--                          ("test-scanner" (line 2, column 1),DecLit "24")]),
--
--  TestLabel "complex-comment" (readTokens " 4// the cow, he jumped! and this happened 0x2 times, afterwards, giant[3] said \"ho,ho,ho! find for fee fum; @!#$!@#^%$^*(&*^(!@$\"\"\"\n\"test_string\"" ~=? Success [])
--  ]
--
--ids = TestList [
--  TestLabel "simple-vars" (readTokens "a Db cd e_D fgh_iJKL _g0g0_gadg3t" ~=? Success []),
--
--  TestLabel "non-keywords" (readTokens "iF If BooLeaN booLean breaK foR For VoiD InTTruE truE inT Class CLASS CONTINuE BreAK fi callouT elsE reTurN" ~=? Success []),
--
--  TestLabel "keywords" (readTokens "break break callout boolean if if int return true void void false else class continue for" ~=? Success []),
--
--  TestLabel "mixed" (readTokens "breakbreak break callout_ boolean if if int return true void void false eLse clasS continue f0r" ~=? Success []),
--
--  TestLabel "invalid-!" (readTokens "break!" ~=? Error ""),
--
--  TestLabel "invalid-3" (readTokens "3l3t3" ~=? Error "")
--  ]
--
--expressions = TestList [
--  TestLabel "simple-expressions" (readTokens "3+2*2 3*5+6 21/3-2" ~=? Success []),
--
--  TestLabel "paren-expressions" (readTokens "(3+6*(2-11)) (21+21/21)/22" ~=? Success [])
--  ]
