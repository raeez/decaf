module ScannerParserTests
where
import Test.HUnit
import Decaf.AST
import Decaf.Scanner
import Decaf.Parser
import Decaf.Tokens
import Text.ParserCombinators.Parsec hiding (spaces)



runScannerParserTests = do runTestTT parserTests


--------------------------------------------
-- parser test
--


parseToks_ :: [Token] -> Report DecafProgram
parseToks_ = parseToksProgram

parseProgram :: String -> Report DecafProgram
parseProgram instr = case scanStringProgram instr of 
  RSuccess val -> parseToks_ val
  RError errstr -> RError "test program error: parser test case fails in scanner phase" 
      

parserTests = TestList [
  arithmeticExpressions
  ]


arithmeticExpressions = TestList [
  TestLabel "missing-closing-brace" (parseProgram "class Program {  void main() { } // missing closing brace \n" ~=? RError "missing closing brace")
  TestLabel "legal01" (parseProgram "class Program {  int i; }" ~=? RSuccess 
                     (DecafProgram 
                      [DecafVarField $ DecafVar DInteger $ DecafIdentifier "i"] 
                      []
                     ))
  TestLabel "legal02" (parseProgram "class Program { int i[10]; }" ~=? RSuccess
                     (DecafProgram 
                      [DecafArrField $ DecafArray DInteger (DecafIdentifier "i") $ DDec 10]
                      []
                     ))
  TestLabel "legal03" (parseProgram "class Program { void main() {  }}" ~=? RSuccess
                       (DecafProgram 
                        [] 
                        [DecafMethod DVoid (DIdentifier "main") [] [] $ DecafBlock [] []]
                       ))
  ]





{-

3+2*2 3*5+6 21/3-2" ~=? RSuccess [7,21,5]),
  TestLabel "paren-expressions" (parseProgram "(3+6*(2-11)) (21+21/21)/22" ~=? RSuccess [-51, 1])


parseExpressionStream :: Parser [Integer]
parseExpressionStream = expr `sepBy` separator

readExpressions :: String -> Report [Integer]
readExpressions input = case parse parseExpressionStream "expression-test-scanner" input of
                          Left err -> RError ("Parser Error!: " ++ show err)
                          Right val -> RSuccess val






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