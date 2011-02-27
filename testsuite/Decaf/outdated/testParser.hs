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
  TestLabel "missing-closing-brace" (parseProgram "class Program {  void main() { } // missing closing brace \n" ~=? RError "missing closing brace"), 
  TestLabel "legal01" (parseProgram "class Program {  int i; }" ~=? RSuccess 
                     (DecafProgram 
                      [DecafVarField $ DecafVar DInteger $ DecafIdentifier "i"] 
                      []
                     )),
  TestLabel "legal02" (parseProgram "class Program { int i[10]; }" ~=? RSuccess
                     (DecafProgram 
                      [DecafArrField $ DecafArray DInteger (DecafIdentifier "i") $ DDec 10]
                      []
                     )),
  TestLabel "legal03" (parseProgram "class Program { void main() {  }}" ~=? RSuccess
                       (DecafProgram 
                        [] 
                        [DecafMethod DVoid (DIdentifier "main") [] $ DecafBlock [] []]
                       )),
  TestLabel "legal04" (parseProgram "class Program {  void main() {    a = -3 * 4 / 6 + (F[b+2] - foo());  }}" ~=? RSuccess
                       (DecafProgram 
                        [] 
                        [DecafMethod DVoid (DIdentifier "main") [] $ DecafBlock 
                         [DecafAssignStm (DecafVarLoc DecafIdentifier "a") DecafEq      -- a = 
                          (DecafBinExpr DecafPlusOp                                     -- +
                           (DecafMinExpr                                                -- -3*4/6
                            (DecafBinExpr DecafDivOp                                    --3*4/6
                             (DecafBinExpr DecafMulOp                                   -- 3*4
                              (DecafLitExpr $ DecafIntLit $ DDec "3")                   ---3
                              (DecafLitExpr $ DecafIntLit $ DDec "4"))                  --4
                             $ DecafLitExpr $DecafIntLit $ DDec "6"))                   --6
                           $ DecafParenExpr 
                           (DecafBinExpr DecafMinOp                                                           -- F[b+2]-foo()
                            (DecafLocExpr 
                             $ DecafArrLoc (DecafIdentifier "F")                                              -- F[]
                               (DecapBinExpr DecafPlusOp (DecafLocExpr $ DecafVarLoc $ DecafIdentifier "b")   -- b +
                                                         (DecafLitExpr $ DecafIntLit $ DDec "2")))            -- 2
                            (DecafMethodExpr $ DecafPureMethodCall (DecafIdentifier "foo") [])))              -- foo()
                         ]
                        ]
                       )),
  

  TestLabel "legal05" (parseProgram "class Program {  int foo() {    return 0;  }  int main() {    return foo();  }}"
                       ~=? RSuccess
                       (DecafProgram 
                        [] 
                        [DecafMethod DInteger (DIdentifier "foo") [] $ DecafBlocek [] 
                          [DecafRetStm $ DecafLitExpr $ DecafIntLit $ DDec "0"],        -- return 0
                         DecafMethod DInteger (DIdentifier "main") [] $ DecafBlock []
                          [DecafRetStm $ DecafMethodExpr $ DecafPureMethodCall (DecafIdentifier "foo") []]   -- return foo()
                         ]
                       )),
  
  TestLabel "legal06" (parseProgram "class Program {  int a;  int add(int a, int b) {    return a + b;  }  int main() {    a = add(2, 3);    return a;  }}"
                       ~=? RSuccess
                       (DecafProgram 
                        [DecafVarField $ DecafVar DInteger $ DecafIdentifier "a"] 
                        [DecafMethod DInteger (DIdentifier "add") 
                          [DecafVar DInterger $ DecafIdentifier "a", DecafVar DInteger $ DecafIdentifier "b"]   -- int a, int b
                          $ DecafBlock [] 
                          [DecafRetStm $ DecafBinExpr DecafPlusOp                                               -- return   + 
                             (DecafLocExpr $ DecafVarLoc $ DecafIdentifier "a")                                 -- a
                             (DecafLocExpr $ DecafVarLoc $ DecafIdentifier "b")],                               -- b
                         DecafMethod DInteger (DIdentifier "main") [] $ DecafBlock []
                          [DecafAssignStm (DecafVarLoc $ DecafIdentifier "a") DecafEq                           -- a =
                           $ DecafMethodExpr $ DecafPureMethodCall (DecafIdentifier "add")                      -- add()
                           [DecafLitExpr $ DecafIntLit $ DDec "2", DecafLitExpr $ DecafIntLit $ DDec "3"],      -- 2,3
                           DecafRetStm $ DecafLocExpr $ DecafVarLof $ DecafIdentifier "a"                       -- return a
                           ]
                         ]
                       )),
  
    -- adding case literally is too slow, using parserBin output
  TestLabel "legal07" (parser "class Program {  int abs(int a) {    int b;    if (a < 0) {      b = -a;    }    else {      b = a;    }    return b;  }  int main() {    return abs(-2);  }}"
                       ~=? 
                       "DecafProgram {fields = [], methods = [DecafMethod {methodType = DInteger, methodID = DecafIdentifier \"abs\", methodArg = [DecafVar {varType = DInteger, varID = DecafIdentifier \"a\"}], methodBody = DecafBlock {blockVars = [DecafVar {varType = DInteger, varID = DecafIdentifier \"b\"}], blockStms = [DecafIfStm (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') (Expr' (DecafBinRelOp DecafLTOp) (Term (DecafLitExpr' (DecafIntLit (DDec \"0\"))) EmptyTerm') EmptyExpr')) (DecafBlock {blockVars = [], blockStms = [DecafAssignStm (DecafVarLoc (DecafIdentifier \"b\")) DecafEq (DecafExpr (Term (DecafMinExpr' (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') EmptyExpr')) EmptyTerm') EmptyExpr')]}) (Just (DecafBlock {blockVars = [], blockStms = [DecafAssignStm (DecafVarLoc (DecafIdentifier \"b\")) DecafEq (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') EmptyExpr')]})),DecafRetStm (Just (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"b\"))) EmptyTerm') EmptyExpr'))]}},DecafMethod {methodType = DInteger, methodID = DecafIdentifier \"main\", methodArg = [], methodBody = DecafBlock {blockVars = [], blockStms = [DecafRetStm (Just (DecafExpr (Term (DecafMethodExpr' (DecafPureMethodCall {methodCallID = DecafIdentifier \"abs\", methodCallArgs = [DecafExpr (Term (DecafMinExpr' (DecafExpr (Term (DecafLitExpr' (DecafIntLit (DDec \"2\"))) EmptyTerm') EmptyExpr')) EmptyTerm') EmptyExpr']})) EmptyTerm') EmptyExpr'))]}}]}"),
  
  TestLabel "legal08" (parser "class Program {  void bar(int a) {    for i = 0, a {      a = a - 1;   }  }   void main() {    bar(10); }}"
                       ~=? 
                       "DecafProgram {fields = [], methods = [DecafMethod {methodType = DVoid, methodID = DecafIdentifier \"bar\", methodArg = [DecafVar {varType = DInteger, varID = DecafIdentifier \"a\"}], methodBody = DecafBlock {blockVars = [], blockStms = [DecafForStm (DecafIdentifier \"i\") (DecafExpr (Term (DecafLitExpr' (DecafIntLit (DDec \"0\"))) EmptyTerm') EmptyExpr') (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') EmptyExpr') (DecafBlock {blockVars = [], blockStms = [DecafAssignStm (DecafVarLoc (DecafIdentifier \"a\")) DecafEq (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') (Expr' (DecafBinArithOp DecafMinOp) (Term (DecafLitExpr' (DecafIntLit (DDec \"1\"))) EmptyTerm') EmptyExpr'))]})]}},DecafMethod {methodType = DVoid, methodID = DecafIdentifier \"main\", methodArg = [], methodBody = DecafBlock {blockVars = [], blockStms = [DecafMethodStm (DecafPureMethodCall {methodCallID = DecafIdentifier \"bar\", methodCallArgs = [DecafExpr (Term (DecafLitExpr' (DecafIntLit (DDec \"10\"))) EmptyTerm') EmptyExpr']})]}}]}"),
                       
  TestLabel "legal09" (parser ""
                       ~=? 
  
                         
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