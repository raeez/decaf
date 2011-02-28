module Decaf.ParserTests
where
import Test.HUnit
import Decaf.AST
import Decaf.Scanner
import Decaf.Parser
import Decaf.Tokens
import Text.ParserCombinators.Parsec hiding (spaces)



--runScannerParserTests = do runTestTT parserTests


--------------------------------------------
-- parser test
--

-- plug in parser here
parser_ :: String -> String
parser_ = parser



parseToks_ :: [Token] -> Report DecafProgram
parseToks_ = parseToksProgram

parseProgram :: String -> Report DecafProgram
parseProgram instr = case scanStringProgram instr of 
  RSuccess val -> parseToks_ val
  RError errstr -> RError "test program error: parser test case fails in scanner phase" 
      



parserTests = TestList [
  selfcoded
  --,testsFromTA
  ]


selfcoded = TestList [
  
  TestLabel "legal01" (parseProgram "class Program {  int i; }" ~=? RSuccess 
                     (DecafProgram 
                      [DecafVarField $ DecafVar DecafInteger $ "i"] 
                      []
                     )),
  
  TestLabel "legal02" (parseProgram "class Program { int i[10]; }" ~=? RSuccess
                     (DecafProgram 
                      [DecafArrField $ DecafArr DecafInteger ("i") $ DecafDec "10"]
                      []
                     )),
  
  TestLabel "legal03" (parseProgram "class Program { void main() {  }}" ~=? RSuccess
                       (DecafProgram 
                        [] 
                        [DecafMethod DecafVoid ("main") [] $ DecafBlock [] []]
                       )),
  
  TestLabel "legal04" (parseProgram "class Program {  void main() {    a = -3 * 4 / 6 + (F[b+2] - foo());  }}" ~=? RSuccess
                       (DecafProgram 
                        [] 
                        [DecafMethod DecafVoid ("main") [] $ DecafBlock []
                         [DecafAssignStm (DecafVarLoc "a") DecafEq      -- a = 
                          (DecafBinExpr (DecafBinArithOp DecafPlusOp)                   -- +
                           (DecafMinExpr                                                -- -3*4/6
                            (DecafBinExpr (DecafBinArithOp DecafDivOp)                                    --3*4/6
                             (DecafBinExpr (DecafBinArithOp DecafMulOp)                                   -- 3*4
                              (DecafLitExpr $ DecafIntLit $ DecafDec "3")                   ---3
                              (DecafLitExpr $ DecafIntLit $ DecafDec "4"))                  --4
                             $ DecafLitExpr $DecafIntLit $ DecafDec "6"))                   --6
                           $ DecafParenExpr 
                           (DecafBinExpr (DecafBinArithOp DecafMinOp)                   -- F[b+2]-foo()
                            (DecafLocExpr 
                             $ DecafArrLoc ("F")                                              -- F[]
                               (DecafBinExpr (DecafBinArithOp DecafPlusOp) (DecafLocExpr $ DecafVarLoc $ "b")   -- b +
                                                                           (DecafLitExpr $ DecafIntLit $ DecafDec "2")))            -- 2
                            (DecafMethodExpr $ DecafPureMethodCall ("foo") [])))              -- foo()
                         ]
                        ]
                       )),
  

  TestLabel "legal05" (parseProgram "class Program {  int foo() {    return 0;  }  int main() {    return foo();  }}"
                       ~=? RSuccess
                       (DecafProgram 
                        [] 
                        [DecafMethod DecafInteger ("foo") [] $ DecafBlock [] 
                          [DecafRetStm $ Just $ DecafLitExpr $ DecafIntLit $ DecafDec "0"],        -- return 0
                         DecafMethod DecafInteger ("main") [] $ DecafBlock []
                          [DecafRetStm $ Just $ DecafMethodExpr $ DecafPureMethodCall ("foo") []]   -- return foo()
                         ]
                       )),
  
  TestLabel "legal06" (parseProgram "class Program {  int a;  int add(int a, int b) {    return a + b;  }  int main() {    a = add(2, 3);    return a;  }}"
                       ~=? RSuccess
                       (DecafProgram 
                        [DecafVarField $ DecafVar DecafInteger $ "a"] 
                        [DecafMethod DecafInteger ("add") 
                          [DecafVar DecafInteger $ "a", DecafVar DecafInteger $ "b"]   -- int a, int b
                          $ DecafBlock [] 
                          [DecafRetStm $ Just $ DecafBinExpr (DecafBinArithOp DecafPlusOp)            -- return   + 
                             (DecafLocExpr $ DecafVarLoc $ "a")                                 -- a
                             (DecafLocExpr $ DecafVarLoc $ "b")],                               -- b
                         DecafMethod DecafInteger ("main") [] $ DecafBlock []
                          [DecafAssignStm (DecafVarLoc $ "a") DecafEq                           -- a =
                           $ DecafMethodExpr $ DecafPureMethodCall ("add")                      -- add()
                           [DecafLitExpr $ DecafIntLit $ DecafDec "2", DecafLitExpr $ DecafIntLit $ DecafDec "3"],      -- 2,3
                           DecafRetStm $ Just $ DecafLocExpr $ DecafVarLoc $ "a"                       -- return a
                           ]
                         ]
                       ))  
  ]








testsFromTA = TestList [
    
    -- still too slow, use automated generation, the following test cases are recorded from parser output 
    TestLabel "illegal-01" (parser_ 
                            "class Program {\n  void main() {\n} // missing closing brace\n"
                            ~=?
                            "\"decaf-scanner-eatNext\" (line 3, column 1):\nunexpected end of input\n"
                           ),
    
    
    
    TestLabel "illegal-02" (parser_ 
                            "class Program {\n  int i\t// missing semicolon\n}\n\n"
                            ~=?
                            "\"decaf-scanner-eatNext\" (line 3, column 1):\nunexpected }\n"
                           ),
    
    
    
    TestLabel "illegal-03" (parser_ 
                            "class Program {\n  int i[];\t// missing array size\n}\n\n"
                            ~=?
                            "\"decaf-scanner-eatNext\" (line 2, column 8):\nunexpected [\n"
                           ),
    
    

    TestLabel "illegal-04" (parser_ 
                            "class Program {\n  void main() {\n    int i = 0;\t// initializers not allowed\n  }\n}\n\n"
                            ~=?
                            "\"decaf-scanner-eatNext\" (line 3, column 5):\nunexpected =\n"
                           ),



  TestLabel "illegal-05" (parser_ 
                          "class Program {\n  void main() {\n  }\n\n  boolean a, b;\t// field decls must be first\n}\n\n"
                          ~=?
                          "\"decaf-scanner-eatNext\" (line 5, column 12):\nunexpected ,\n"
                         ),
  
  
  
  TestLabel "illegal-06" (parser_ 
                          "class Program {\n  int a[2+3];\t// bad array decl\n}\n\n"
                          ~=?
                          "\"decaf-scanner-eatNext\" (line 2, column 8):\nunexpected [\n"
                         ),
  
  
  
  TestLabel "illegal-07" (parser_ 
                          "class Program {\n  void main() {\n    int callout;\t// callout is a reserved word\n  }\n}\n\n"
                          ~=?
                          "\"decaf-scanner-eatNext\" (line 3, column 5):\nunexpected callout\n"
                         ),
  
  
  
  TestLabel "illegal-08" (parser_ 
                          "class Program {\n  void main() {\n    int b;\n    b = 2 + 7 = 12;\t// bad expression\n  }\n}\n\n"
                          ~=?
                          "\"decaf-scanner-eatNext\" (line 4, column 5):\nunexpected IDENTIFIER b\n"
                         ),
  
  
  
  TestLabel "illegal-09" (parser_ 
                          "class Program {\n  void main() {\n    Int b;\t// should be int\n  }\n}\n\n"
                          ~=?
                          "\"decaf-scanner-eatNext\" (line 3, column 5):\nunexpected IDENTIFIER Int\n"
                         ),
  
  
  
  TestLabel "illegal-10" (parser_ 
                          "class Program {\n  void foo() {\n    int a;\n    a = 0;\n    // missing closing brace\n  void main() {\n    int b;\n  }\n}\n\n"
                          ~=?
                          "\"decaf-scanner-eatNext\" (line 6, column 3):\nunexpected void\n"
                         ),
  
  
  
  TestLabel "illegal-11" (parser_ 
                          "class Program {\n  void foo() {\n    int a;\n    a = 0;\n  }\n  void main() {\n    int;\t// missing variable name\n  }\n}\n\n"
                          ~=?
                          "\"decaf-scanner-eatNext\" (line 7, column 5):\nunexpected ;\n"
                         ),
  
  
  
  TestLabel "illegal-12" (parser_ 
                          "class Program {\n  void foo() {\n    int a;\n    a = 0;\n  }\n  void main() {\n    int a b;\t// missing comma\n  }\n}\n\n"
                          ~=?
                          "\"decaf-scanner-eatNext\" (line 7, column 5):\nunexpected IDENTIFIER b\n"
                         ),
  
  
  
  TestLabel "illegal-13" (parser_ 
                          "class Program {\n  main() {\t// no return type\n  }\n}\n\n"
                          ~=?
                          "\"decaf-scanner-eatNext\" (line 2, column 3):\nunexpected IDENTIFIER main\n"
                         ),
  
  
  
  TestLabel "illegal-14" (parser_ 
                          "class Program {\n  int main(a) {\t// no parameter type\n  }\n}\n\n"
                          ~=?
                          "\"decaf-scanner-eatNext\" (line 2, column 12):\nunexpected IDENTIFIER a\n"
                         ),
  
  
  
  TestLabel "illegal-15" (parser_ 
                          "class Program {\n  int main(int a) {\n    foo(int);\t// type in function call\n  }\n}\n\n"
                          ~=?
                          "\"decaf-scanner-eatNext\" (line 3, column 5):\nunexpected IDENTIFIER foo\n"
                         ),
  
  
  
  TestLabel "illegal-16" (parser_ 
                          "class Program {\n  void main() {\n    if () { // no condition\n    }\n  }\n}\n\n"
                          ~=?
                          "\"decaf-scanner-eatNext\" (line 3, column 9):\nunexpected )\n"
                         ),
  
  
  
  TestLabel "illegal-17" (parser_ 
                          "class Program {\n  void main() {\n    0xcafe();\t// bad identifier\n  }\n}\n\n"
                          ~=?
                          "\"decaf-scanner-eatNext\" (line 3, column 5):\nunexpected INTLITERAL 0xcafe\n"
                         ),
  
  
  
  TestLabel "illegal-18" (parser_ 
                          "class Program {\n  void main() {\n    callout(5);\t// first arg must be a string\n  }\n}\n\n"
                          ~=?
                          "\"decaf-scanner-eatNext\" (line 3, column 5):\nunexpected callout\n"
                         ),
  
  
  
  TestLabel "illegal-19" (parser_ 
                          "class Program {\n  int A[0xA];\n\n  void bar() {\n    forpar i = 0, 0xA {\n      A[i] = i;\n    }\n  }\n\n  void main() {\n    bar();\n  }\n}\n"
                          ~=?
                          "\"decaf-scanner-eatNext\" (line 5, column 5):\nunexpected IDENTIFIER forpar\n"
                         ),
  
  
  
  TestLabel "illegal-20" (parser_ 
                          "class Program {\n  int A[10];\n\n  void bar() {\n    forpar i = 0, 10 {\n      A[i] = i;\n    }\n  }\n\n  void main() {\n    bar();\n  }\n}\n"
                          ~=?
                          "\"decaf-scanner-eatNext\" (line 5, column 5):\nunexpected IDENTIFIER forpar\n"
                         ),
  


  TestLabel "legal-01" (parser_ 
                        "class Program {\n  int i;\n}\n"
                        ~=?
                        "DecafProgram {fields = [DecafVarField (DecafVar {varType = DecafInteger, varID = DecafIdentifier \"i\"})], methods = []}\n"
                       ),



  TestLabel "legal-02" (parser_ 
                        "class Program {\n  int i[10];\n}\n"
                        ~=?
                        "DecafProgram {fields = [DecafArrField (DecafArray {arrayType = DecafInteger, arrayID = DecafIdentifier \"i\", arrayLength = DecafDec \"10\"})], methods = []}\n"
                       ),
  


  TestLabel "legal-03" (parser_ 
                        "class Program {\n  void main() {\n  }\n}\n"
                        ~=?
                        "DecafProgram {fields = [], methods = [DecafMethod {methodType = DecafVoid, methodID = DecafIdentifier \"main\", methodArg = [], methodBody = DecafBlock {blockVars = [], blockStms = []}}]}\n"
                       ),
  
  
  
  TestLabel "legal-04" (parser_ 
                        "class Program {\n  void main() {\n    a = -3 * 4 / 6 + (F[b+2] - foo());\n  }\n}\n"
                        ~=?
                        "DecafProgram {fields = [], methods = [DecafMethod {methodType = DecafVoid, methodID = DecafIdentifier \"main\", methodArg = [], methodBody = DecafBlock {blockVars = [], blockStms = [DecafAssignStm (DecafVarLoc (DecafIdentifier \"a\")) DecafEq (DecafExpr (Term (DecafMinExpr' (DecafExpr (Term (DecafLitExpr' (DecafIntLit (DecafDec \"3\"))) (Term' (DecafBinArithOp DecafMulOp) (DecafLitExpr' (DecafIntLit (DecafDec \"4\"))) (Term' (DecafBinArithOp DecafDivOp) (DecafLitExpr' (DecafIntLit (DecafDec \"6\"))) EmptyTerm'))) (Expr' (DecafBinArithOp DecafPlusOp) (Term (DecafParenExpr' (DecafExpr (Term (DecafLocExpr' (DecafArrLoc (DecafIdentifier \"F\") (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"b\"))) EmptyTerm') (Expr' (DecafBinArithOp DecafPlusOp) (Term (DecafLitExpr' (DecafIntLit (DecafDec \"2\"))) EmptyTerm') EmptyExpr')))) EmptyTerm') (Expr' (DecafBinArithOp DecafMinOp) (Term (DecafMethodExpr' (DecafPureMethodCall {methodCallID = DecafIdentifier \"foo\", methodCallArgs = []})) EmptyTerm') EmptyExpr'))) EmptyTerm') EmptyExpr'))) EmptyTerm') EmptyExpr')]}}]}\n"
                       ),
  
  
  
  TestLabel "legal-05" (parser_ 
                        "class Program {\n  int foo() {\n    return 0;\n  }\n\n  int main() {\n    return foo();\n  }\n}\n"
                        ~=?
                        "DecafProgram {fields = [], methods = [DecafMethod {methodType = DecafInteger, methodID = DecafIdentifier \"foo\", methodArg = [], methodBody = DecafBlock {blockVars = [], blockStms = [DecafRetStm (Just (DecafExpr (Term (DecafLitExpr' (DecafIntLit (DecafDec \"0\"))) EmptyTerm') EmptyExpr'))]}},DecafMethod {methodType = DecafInteger, methodID = DecafIdentifier \"main\", methodArg = [], methodBody = DecafBlock {blockVars = [], blockStms = [DecafRetStm (Just (DecafExpr (Term (DecafMethodExpr' (DecafPureMethodCall {methodCallID = DecafIdentifier \"foo\", methodCallArgs = []})) EmptyTerm') EmptyExpr'))]}}]}\n"
                       ),
  
  
  
  TestLabel "legal-06" (parser_ 
                        "class Program {\n  int a;\n\n  int add(int a, int b) {\n    return a + b;\n  }\n\n  int main() {\n    a = add(2, 3);\n    return a;\n  }\n}\n"
                        ~=?
                        "DecafProgram {fields = [DecafVarField (DecafVar {varType = DecafInteger, varID = DecafIdentifier \"a\"})], methods = [DecafMethod {methodType = DecafInteger, methodID = DecafIdentifier \"add\", methodArg = [DecafVar {varType = DecafInteger, varID = DecafIdentifier \"a\"},DecafVar {varType = DecafInteger, varID = DecafIdentifier \"b\"}], methodBody = DecafBlock {blockVars = [], blockStms = [DecafRetStm (Just (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') (Expr' (DecafBinArithOp DecafPlusOp) (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"b\"))) EmptyTerm') EmptyExpr')))]}},DecafMethod {methodType = DecafInteger, methodID = DecafIdentifier \"main\", methodArg = [], methodBody = DecafBlock {blockVars = [], blockStms = [DecafAssignStm (DecafVarLoc (DecafIdentifier \"a\")) DecafEq (DecafExpr (Term (DecafMethodExpr' (DecafPureMethodCall {methodCallID = DecafIdentifier \"add\", methodCallArgs = [DecafExpr (Term (DecafLitExpr' (DecafIntLit (DecafDec \"2\"))) EmptyTerm') EmptyExpr',DecafExpr (Term (DecafLitExpr' (DecafIntLit (DecafDec \"3\"))) EmptyTerm') EmptyExpr']})) EmptyTerm') EmptyExpr'),DecafRetStm (Just (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') EmptyExpr'))]}}]}\n"
                       ),
  
  
  
  TestLabel "legal-07" (parser_ 
                        "class Program {\n  int abs(int a) {\n    int b;\n    if (a < 0) {\n      b = -a;\n    }\n    else {\n      b = a;\n    }\n\n    return b;\n  }\n\n  int main() {\n    return abs(-2);\n  }\n}\n"
                        ~=?
                        "DecafProgram {fields = [], methods = [DecafMethod {methodType = DecafInteger, methodID = DecafIdentifier \"abs\", methodArg = [DecafVar {varType = DecafInteger, varID = DecafIdentifier \"a\"}], methodBody = DecafBlock {blockVars = [DecafVar {varType = DecafInteger, varID = DecafIdentifier \"b\"}], blockStms = [DecafIfStm (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') (Expr' (DecafBinRelOp DecafLTOp) (Term (DecafLitExpr' (DecafIntLit (DecafDec \"0\"))) EmptyTerm') EmptyExpr')) (DecafBlock {blockVars = [], blockStms = [DecafAssignStm (DecafVarLoc (DecafIdentifier \"b\")) DecafEq (DecafExpr (Term (DecafMinExpr' (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') EmptyExpr')) EmptyTerm') EmptyExpr')]}) (Just (DecafBlock {blockVars = [], blockStms = [DecafAssignStm (DecafVarLoc (DecafIdentifier \"b\")) DecafEq (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') EmptyExpr')]})),DecafRetStm (Just (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"b\"))) EmptyTerm') EmptyExpr'))]}},DecafMethod {methodType = DecafInteger, methodID = DecafIdentifier \"main\", methodArg = [], methodBody = DecafBlock {blockVars = [], blockStms = [DecafRetStm (Just (DecafExpr (Term (DecafMethodExpr' (DecafPureMethodCall {methodCallID = DecafIdentifier \"abs\", methodCallArgs = [DecafExpr (Term (DecafMinExpr' (DecafExpr (Term (DecafLitExpr' (DecafIntLit (DecafDec \"2\"))) EmptyTerm') EmptyExpr')) EmptyTerm') EmptyExpr']})) EmptyTerm') EmptyExpr'))]}}]}\n"
                       ),
  
  
  
  TestLabel "legal-08" (parser_ 
                        "class Program {\n  void bar(int a) {\n    for i = 0, a {\n      a = a - 1;\n    }\n  }\n\n  void main() {\n    bar(10);\n  }\n}\n"
                        ~=?
                        "DecafProgram {fields = [], methods = [DecafMethod {methodType = DecafVoid, methodID = DecafIdentifier \"bar\", methodArg = [DecafVar {varType = DecafInteger, varID = DecafIdentifier \"a\"}], methodBody = DecafBlock {blockVars = [], blockStms = [DecafForStm (DecafIdentifier \"i\") (DecafExpr (Term (DecafLitExpr' (DecafIntLit (DecafDec \"0\"))) EmptyTerm') EmptyExpr') (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') EmptyExpr') (DecafBlock {blockVars = [], blockStms = [DecafAssignStm (DecafVarLoc (DecafIdentifier \"a\")) DecafEq (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') (Expr' (DecafBinArithOp DecafMinOp) (Term (DecafLitExpr' (DecafIntLit (DecafDec \"1\"))) EmptyTerm') EmptyExpr'))]})]}},DecafMethod {methodType = DecafVoid, methodID = DecafIdentifier \"main\", methodArg = [], methodBody = DecafBlock {blockVars = [], blockStms = [DecafMethodStm (DecafPureMethodCall {methodCallID = DecafIdentifier \"bar\", methodCallArgs = [DecafExpr (Term (DecafLitExpr' (DecafIntLit (DecafDec \"10\"))) EmptyTerm') EmptyExpr']})]}}]}\n"
                       ),
  
  
  
  TestLabel "legal-09" (parser_ 
                        "class Program {\n  void bar(int a) {\n    for i = 0, a {\n      callout(\"printf\", \"%d\\n\", i);\n    }\n  }\n\n  void main() {\n    bar(10);\n  }\n}\n"
                        ~=?
                        "DecafProgram {fields = [], methods = [DecafMethod {methodType = DecafVoid, methodID = DecafIdentifier \"bar\", methodArg = [DecafVar {varType = DecafInteger, varID = DecafIdentifier \"a\"}], methodBody = DecafBlock {blockVars = [], blockStms = [DecafForStm (DecafIdentifier \"i\") (DecafExpr (Term (DecafLitExpr' (DecafIntLit (DecafDec \"0\"))) EmptyTerm') EmptyExpr') (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') EmptyExpr') (DecafBlock {blockVars = [], blockStms = [DecafMethodStm (DecafMethodCallout {methodCalloutID = DStr \"printf\", methodCalloutArgs = [DecafCalloutArgStr (DStr \"%d\\n\"),DecafCalloutArgExpr (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"i\"))) EmptyTerm') EmptyExpr')]})]})]}},DecafMethod {methodType = DecafVoid, methodID = DecafIdentifier \"main\", methodArg = [], methodBody = DecafBlock {blockVars = [], blockStms = [DecafMethodStm (DecafPureMethodCall {methodCallID = DecafIdentifier \"bar\", methodCallArgs = [DecafExpr (Term (DecafLitExpr' (DecafIntLit (DecafDec \"10\"))) EmptyTerm') EmptyExpr']})]}}]}\n"
                       ),
  
  
  
  TestLabel "legal-10" (parser_ 
                        "class Program {\n  void bar(int a) {\n\n    for i = 0, a {\n      callout(\"printf\", \"%d\\n\", i);\n      if (a == 4) {\n\tbreak;\n      }\n    }\n  }\n\n  void main() {\n    bar(10);\n  }\n}\n"
                        ~=?
                        "DecafProgram {fields = [], methods = [DecafMethod {methodType = DecafVoid, methodID = DecafIdentifier \"bar\", methodArg = [DecafVar {varType = DecafInteger, varID = DecafIdentifier \"a\"}], methodBody = DecafBlock {blockVars = [], blockStms = [DecafForStm (DecafIdentifier \"i\") (DecafExpr (Term (DecafLitExpr' (DecafIntLit (DecafDec \"0\"))) EmptyTerm') EmptyExpr') (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') EmptyExpr') (DecafBlock {blockVars = [], blockStms = [DecafMethodStm (DecafMethodCallout {methodCalloutID = DStr \"printf\", methodCalloutArgs = [DecafCalloutArgStr (DStr \"%d\\n\"),DecafCalloutArgExpr (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"i\"))) EmptyTerm') EmptyExpr')]}),DecafIfStm (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') (Expr' (DecafBinEqOp DecafEqOp) (Term (DecafLitExpr' (DecafIntLit (DecafDec \"4\"))) EmptyTerm') EmptyExpr')) (DecafBlock {blockVars = [], blockStms = [DecafBreakStm]}) Nothing]})]}},DecafMethod {methodType = DecafVoid, methodID = DecafIdentifier \"main\", methodArg = [], methodBody = DecafBlock {blockVars = [], blockStms = [DecafMethodStm (DecafPureMethodCall {methodCallID = DecafIdentifier \"bar\", methodCallArgs = [DecafExpr (Term (DecafLitExpr' (DecafIntLit (DecafDec \"10\"))) EmptyTerm') EmptyExpr']})]}}]}\n"
                       ),
  
  
  
  TestLabel "legal-11" (parser_ 
                        "class Program {\n  int A[10];\n\n  void bar() {\n    for i = 0, 10 {\n      A[i] = i;\n    }\n  }\n\n  void main() {\n    bar();\n  }\n}\n"
                        ~=?
                        "DecafProgram {fields = [DecafArrField (DecafArray {arrayType = DecafInteger, arrayID = DecafIdentifier \"A\", arrayLength = DecafDec \"10\"})], methods = [DecafMethod {methodType = DecafVoid, methodID = DecafIdentifier \"bar\", methodArg = [], methodBody = DecafBlock {blockVars = [], blockStms = [DecafForStm (DecafIdentifier \"i\") (DecafExpr (Term (DecafLitExpr' (DecafIntLit (DecafDec \"0\"))) EmptyTerm') EmptyExpr') (DecafExpr (Term (DecafLitExpr' (DecafIntLit (DecafDec \"10\"))) EmptyTerm') EmptyExpr') (DecafBlock {blockVars = [], blockStms = [DecafAssignStm (DecafArrLoc (DecafIdentifier \"A\") (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"i\"))) EmptyTerm') EmptyExpr')) DecafEq (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"i\"))) EmptyTerm') EmptyExpr')]})]}},DecafMethod {methodType = DecafVoid, methodID = DecafIdentifier \"main\", methodArg = [], methodBody = DecafBlock {blockVars = [], blockStms = [DecafMethodStm (DecafPureMethodCall {methodCallID = DecafIdentifier \"bar\", methodCallArgs = []})]}}]}\n"
                       ),
  
  
  
  TestLabel "legal-12" (parser_ 
                        "class Program {\n  int A[0xA];\n\n  void bar() {\n    for i = 0, 0xA {\n      A[i] = i;\n    }\n  }\n\n  void main() {\n    bar();\n  }\n}\n"
                        ~=?
                        "DecafProgram {fields = [DecafArrField (DecafArray {arrayType = DecafInteger, arrayID = DecafIdentifier \"A\", arrayLength = DHex \"A\"})], methods = [DecafMethod {methodType = DecafVoid, methodID = DecafIdentifier \"bar\", methodArg = [], methodBody = DecafBlock {blockVars = [], blockStms = [DecafForStm (DecafIdentifier \"i\") (DecafExpr (Term (DecafLitExpr' (DecafIntLit (DecafDec \"0\"))) EmptyTerm') EmptyExpr') (DecafExpr (Term (DecafLitExpr' (DecafIntLit (DHex \"A\"))) EmptyTerm') EmptyExpr') (DecafBlock {blockVars = [], blockStms = [DecafAssignStm (DecafArrLoc (DecafIdentifier \"A\") (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"i\"))) EmptyTerm') EmptyExpr')) DecafEq (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"i\"))) EmptyTerm') EmptyExpr')]})]}},DecafMethod {methodType = DecafVoid, methodID = DecafIdentifier \"main\", methodArg = [], methodBody = DecafBlock {blockVars = [], blockStms = [DecafMethodStm (DecafPureMethodCall {methodCallID = DecafIdentifier \"bar\", methodCallArgs = []})]}}]}\n"
                       ),
  
  
  
  TestLabel "legal-13" (parser_ 
                        "class Program {\n  int abs(int a) {\n    if (a < 0) {\n      int b;\n      b = -a;\n      return b;\n    }\n\n    return a;\n  }\n\n  void main() {\n    abs(-5);\n  }\n}\n"
                        ~=?
                        "DecafProgram {fields = [], methods = [DecafMethod {methodType = DecafInteger, methodID = DecafIdentifier \"abs\", methodArg = [DecafVar {varType = DecafInteger, varID = DecafIdentifier \"a\"}], methodBody = DecafBlock {blockVars = [], blockStms = [DecafIfStm (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') (Expr' (DecafBinRelOp DecafLTOp) (Term (DecafLitExpr' (DecafIntLit (DecafDec \"0\"))) EmptyTerm') EmptyExpr')) (DecafBlock {blockVars = [DecafVar {varType = DecafInteger, varID = DecafIdentifier \"b\"}], blockStms = [DecafAssignStm (DecafVarLoc (DecafIdentifier \"b\")) DecafEq (DecafExpr (Term (DecafMinExpr' (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') EmptyExpr')) EmptyTerm') EmptyExpr'),DecafRetStm (Just (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"b\"))) EmptyTerm') EmptyExpr'))]}) Nothing,DecafRetStm (Just (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') EmptyExpr'))]}},DecafMethod {methodType = DecafVoid, methodID = DecafIdentifier \"main\", methodArg = [], methodBody = DecafBlock {blockVars = [], blockStms = [DecafMethodStm (DecafPureMethodCall {methodCallID = DecafIdentifier \"abs\", methodCallArgs = [DecafExpr (Term (DecafMinExpr' (DecafExpr (Term (DecafLitExpr' (DecafIntLit (DecafDec \"5\"))) EmptyTerm') EmptyExpr')) EmptyTerm') EmptyExpr']})]}}]}\n"
                       ),
  
  
  
  TestLabel "legal-14" (parser_ 
                        "class Program {\n  int abs(int a) {\n    boolean b;\n    b = a < 0;\n\n    if (b) {\n      return -a;\n    }\n    else {\n      return a;\n    }\n  }\n\n  void main() {\n    abs(-5);\n  }\n}\n"
                        ~=?
                        "DecafProgram {fields = [], methods = [DecafMethod {methodType = DecafInteger, methodID = DecafIdentifier \"abs\", methodArg = [DecafVar {varType = DecafInteger, varID = DecafIdentifier \"a\"}], methodBody = DecafBlock {blockVars = [DecafVar {varType = DBoolean, varID = DecafIdentifier \"b\"}], blockStms = [DecafAssignStm (DecafVarLoc (DecafIdentifier \"b\")) DecafEq (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') (Expr' (DecafBinRelOp DecafLTOp) (Term (DecafLitExpr' (DecafIntLit (DecafDec \"0\"))) EmptyTerm') EmptyExpr')),DecafIfStm (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"b\"))) EmptyTerm') EmptyExpr') (DecafBlock {blockVars = [], blockStms = [DecafRetStm (Just (DecafExpr (Term (DecafMinExpr' (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') EmptyExpr')) EmptyTerm') EmptyExpr'))]}) (Just (DecafBlock {blockVars = [], blockStms = [DecafRetStm (Just (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') EmptyExpr'))]}))]}},DecafMethod {methodType = DecafVoid, methodID = DecafIdentifier \"main\", methodArg = [], methodBody = DecafBlock {blockVars = [], blockStms = [DecafMethodStm (DecafPureMethodCall {methodCallID = DecafIdentifier \"abs\", methodCallArgs = [DecafExpr (Term (DecafMinExpr' (DecafExpr (Term (DecafLitExpr' (DecafIntLit (DecafDec \"5\"))) EmptyTerm') EmptyExpr')) EmptyTerm') EmptyExpr']})]}}]}\n"
                       ),
  
  
  
  TestLabel "legal-15" (parser_ 
                        "class Program {\n  boolean b;\n  \n  int abs(int a) {\n    b = a < 0;\n\n    if (b) {\n      return -a;\n    }\n    else {\n      return a;\n    }\n  }\n\n  void main() {\n    abs(-5);\n  }\n}\n"
                        ~=?
                        "DecafProgram {fields = [DecafVarField (DecafVar {varType = DBoolean, varID = DecafIdentifier \"b\"})], methods = [DecafMethod {methodType = DecafInteger, methodID = DecafIdentifier \"abs\", methodArg = [DecafVar {varType = DecafInteger, varID = DecafIdentifier \"a\"}], methodBody = DecafBlock {blockVars = [], blockStms = [DecafAssignStm (DecafVarLoc (DecafIdentifier \"b\")) DecafEq (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') (Expr' (DecafBinRelOp DecafLTOp) (Term (DecafLitExpr' (DecafIntLit (DecafDec \"0\"))) EmptyTerm') EmptyExpr')),DecafIfStm (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"b\"))) EmptyTerm') EmptyExpr') (DecafBlock {blockVars = [], blockStms = [DecafRetStm (Just (DecafExpr (Term (DecafMinExpr' (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') EmptyExpr')) EmptyTerm') EmptyExpr'))]}) (Just (DecafBlock {blockVars = [], blockStms = [DecafRetStm (Just (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') EmptyExpr'))]}))]}},DecafMethod {methodType = DecafVoid, methodID = DecafIdentifier \"main\", methodArg = [], methodBody = DecafBlock {blockVars = [], blockStms = [DecafMethodStm (DecafPureMethodCall {methodCallID = DecafIdentifier \"abs\", methodCallArgs = [DecafExpr (Term (DecafMinExpr' (DecafExpr (Term (DecafLitExpr' (DecafIntLit (DecafDec \"5\"))) EmptyTerm') EmptyExpr')) EmptyTerm') EmptyExpr']})]}}]}\n"
                       ),
  
  
  
  TestLabel "legal-16" (parser_ 
                        "class Program {\n  void main() {\n    int char;\n    char = 'a';\n    callout(\"printf\", \"%c\\n\", char);\n  }\n}\n"
                        ~=?
                        "DecafProgram {fields = [], methods = [DecafMethod {methodType = DecafVoid, methodID = DecafIdentifier \"main\", methodArg = [], methodBody = DecafBlock {blockVars = [DecafVar {varType = DecafInteger, varID = DecafIdentifier \"char\"}], blockStms = [DecafAssignStm (DecafVarLoc (DecafIdentifier \"char\")) DecafEq (DecafExpr (Term (DecafLitExpr' (DecafCharLit (DChar 'a'))) EmptyTerm') EmptyExpr'),DecafMethodStm (DecafMethodCallout {methodCalloutID = DStr \"printf\", methodCalloutArgs = [DecafCalloutArgStr (DStr \"%c\\n\"),DecafCalloutArgExpr (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"char\"))) EmptyTerm') EmptyExpr')]})]}}]}\n"
                       ),
  
  
  
  TestLabel "legal-17" (parser_ 
                        "class Program {\n  void main() {\n    int a;\n    int a;\t// semanitcally bad, but gramatically ok\n  }\n}\n"
                        ~=?
                        "DecafProgram {fields = [], methods = [DecafMethod {methodType = DecafVoid, methodID = DecafIdentifier \"main\", methodArg = [], methodBody = DecafBlock {blockVars = [DecafVar {varType = DecafInteger, varID = DecafIdentifier \"a\"},DecafVar {varType = DecafInteger, varID = DecafIdentifier \"a\"}], blockStms = []}}]}\n"
                       ),
  
  
  
  TestLabel "legal-18" (parser_ 
                        "class Program {\n  void main() {\n    foo();\t// semanitcally bad, but gramatically ok\n  }\n\n  void foo() {}\n}\n"
                        ~=?
                        "DecafProgram {fields = [], methods = [DecafMethod {methodType = DecafVoid, methodID = DecafIdentifier \"main\", methodArg = [], methodBody = DecafBlock {blockVars = [], blockStms = [DecafMethodStm (DecafPureMethodCall {methodCallID = DecafIdentifier \"foo\", methodCallArgs = []})]}},DecafMethod {methodType = DecafVoid, methodID = DecafIdentifier \"foo\", methodArg = [], methodBody = DecafBlock {blockVars = [], blockStms = []}}]}\n"
                       )
  
  
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
--dTok2DLit d = DecafDec 0   -- temp -J
  

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






{-

  
    -- adding case literally is too slow, using parserBin output
    TestLabel "legal07" (parser_ "class Program {  int abs(int a) {    int b;    if (a < 0) {      b = -a;    }    else {      b = a;    }    return b;  }  int main() {    return abs(-2);  }}"
                         ~=? 
                         "DecafProgram {fields = [], methods = [DecafMethod {methodType = DecafInteger, methodID = DecafIdentifier \"abs\", methodArg = [DecafVar {varType = DecafInteger, varID = DecafIdentifier \"a\"}], methodBody = DecafBlock {blockVars = [DecafVar {varType = DecafInteger, varID = DecafIdentifier \"b\"}], blockStms = [DecafIfStm (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') (Expr' (DecafBinRelOp DecafLTOp) (Term (DecafLitExpr' (DecafIntLit (DecafDec \"0\"))) EmptyTerm') EmptyExpr')) (DecafBlock {blockVars = [], blockStms = [DecafAssignStm (DecafVarLoc (DecafIdentifier \"b\")) DecafEq (DecafExpr (Term (DecafMinExpr' (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') EmptyExpr')) EmptyTerm') EmptyExpr')]}) (Just (DecafBlock {blockVars = [], blockStms = [DecafAssignStm (DecafVarLoc (DecafIdentifier \"b\")) DecafEq (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') EmptyExpr')]})),DecafRetStm (Just (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"b\"))) EmptyTerm') EmptyExpr'))]}},DecafMethod {methodType = DecafInteger, methodID = DecafIdentifier \"main\", methodArg = [], methodBody = DecafBlock {blockVars = [], blockStms = [DecafRetStm (Just (DecafExpr (Term (DecafMethodExpr' (DecafPureMethodCall {methodCallID = DecafIdentifier \"abs\", methodCallArgs = [DecafExpr (Term (DecafMinExpr' (DecafExpr (Term (DecafLitExpr' (DecafIntLit (DecafDec \"2\"))) EmptyTerm') EmptyExpr')) EmptyTerm') EmptyExpr']})) EmptyTerm') EmptyExpr'))]}}]}"),
    
    TestLabel "legal08" (parser_ "class Program {  void bar(int a) {    for i = 0, a {      a = a - 1;   }  }   void main() {    bar(10); }}"
                         ~=? 
                         "DecafProgram {fields = [], methods = [DecafMethod {methodType = DecafVoid, methodID = DecafIdentifier \"bar\", methodArg = [DecafVar {varType = DecafInteger, varID = DecafIdentifier \"a\"}], methodBody = DecafBlock {blockVars = [], blockStms = [DecafForStm (DecafIdentifier \"i\") (DecafExpr (Term (DecafLitExpr' (DecafIntLit (DecafDec \"0\"))) EmptyTerm') EmptyExpr') (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') EmptyExpr') (DecafBlock {blockVars = [], blockStms = [DecafAssignStm (DecafVarLoc (DecafIdentifier \"a\")) DecafEq (DecafExpr (Term (DecafLocExpr' (DecafVarLoc (DecafIdentifier \"a\"))) EmptyTerm') (Expr' (DecafBinArithOp DecafMinOp) (Term (DecafLitExpr' (DecafIntLit (DecafDec \"1\"))) EmptyTerm') EmptyExpr'))]})]}},DecafMethod {methodType = DVoid, methodID = DecafIdentifier \"main\", methodArg = [], methodBody = DecafBlock {blockVars = [], blockStms = [DecafMethodStm (DecafPureMethodCall {methodCallID = DecafIdentifier \"bar\", methodCallArgs = [DecafExpr (Term (DecafLitExpr' (DecafIntLit (DecafDec \"10\"))) EmptyTerm') EmptyExpr']})]}}]}"),
    
-}