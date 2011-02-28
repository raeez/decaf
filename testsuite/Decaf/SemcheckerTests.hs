module Decaf.SemcheckerTests
where
import Test.HUnit
import Decaf.Scanner
import Decaf.Parser
--import TestRoot
import Text.ParserCombinators.Parsec hiding (spaces)



-- runSemanticsTests = do runTestTT semcheckerTests
  
-- plug in semchecker here                      
semchecker_ :: String -> Bool                      
semchecker_ instr = False


semcheckerTests = TestList [
  testsFromTA
  ]
                


--------------------------------------------
-- Semantics Checking Test
--


testsFromTA = TestList [
  TestLabel "illegal-01.dcf" (semchecker_ 
                              "class Program {\n  void main() {\n    int x;\n    boolean x;\t// identifier declared twice\n  }\t\t\t\t\t\t\t\t\n}\n"
                              ~=? False
                             ),
  
  
  
  TestLabel "illegal-02.dcf" (semchecker_ 
                              "class Program {\n  void main() {\n    a = 34;\t// identifier used before being declared\n  }\t\t\t\t\t\t\t\t\n}\t\n"
                              ~=? False
                             ),
  
  
  
  TestLabel "illegal-03.dcf" (semchecker_ 
                              "// No main method\nclass Program {\n  void foo() {\n  }\n  void bar() {\n  }\n}\t\n"
                              ~=? False
                             ),
  
  
  
  TestLabel "illegal-04.dcf" (semchecker_ 
                              "class Program {\n  int a[0];\t// bad array size\n  void main() {\n  }\t\t\t\t\t\t\t\n}\n"
                              ~=? False
                             ),
  
  
  
  TestLabel "illegal-05.dcf" (semchecker_ 
                              "class Program {\n  int foo(int i, boolean b) {\n    return 4;\n  }\n\n  void main() {\n    int x;\n    x = foo(34, true, true);\t// argument mismatch\n  }\t\t\t\t\t\t\t\t\n}\t\n"
                              ~=? False
                             ),
  
  
  
  TestLabel "illegal-06.dcf" (semchecker_ 
                              "class Program {\n  int foo(int i, boolean b) {\n    return 3;\n  }\n\n  void main() {\n    int x;\n    x = foo(34, 35);\t// types don't match signature\n  }\t\t\t\t\t\t\t\t\n}\t\n"
                              ~=? False
                             ),
  
  
  
  TestLabel "illegal-07.dcf" (semchecker_ 
                              "class Program {\n  void foo(int i, boolean b) {\n    return 3;\t// should not return value;\n  }\n  void main() {\n  }\t\t\t\t\t\t\t\t\n}\n"
                              ~=? False
                             ),
  
  
  
  TestLabel "illegal-08.dcf" (semchecker_ 
                              "class Program {\n  int foo(int i, boolean b) {\n    return true;\t// return value has wrong type\n  }\n  void main() {\n    int x;\n    x = foo(34, true);\n  }\t\t\t\t\t\t\t\n}\n"
                              ~=? False
                             ),
  

  
  TestLabel "illegal-09.dcf" (semchecker_ 
                              "class Program {\n  int a[10];\n  boolean b;\n\n  void main() {\n    a[b] = 25;\t// array index has wrong type\n  }\t\t\t\t\t\t\t\t\n}\n"
                              ~=? False
                             ),
  
  
  
  TestLabel "illegal-10.dcf" (semchecker_ 
                              "class Program {\n  int a[10];\n\n  void main() {\n    a[1] = a;\t// bad type, rhs should be an int\n  }\t\t\t\t\t\t\t\t\n}\n"
                              ~=? False
                             ),
  
  
  
  TestLabel "illegal-11.dcf" (semchecker_ 
                              "class Program {\n  int a[10];\n\n  void main() {\n    if (a[3]) {}\t// condition should be a boolean\n  }\t\t\t\t\t\t\t\t\n}\n"
                              ~=? False
                             ),
  
  
  
  TestLabel "illegal-12.dcf" (semchecker_ 
                              "class Program {\n  void main() {\n    for x = false, 10 {   //initial condition must be an int\n    }\n  }\t\t\t\t\t\t\t\n}\n"
                              ~=? False
                             ),
  
  
  
  TestLabel "illegal-13.dcf" (semchecker_ 
                              "class Program {\n  int a[10];\n  int i;\n\n  void main() {\n    a[3] = i < 35;\t// rhs should be an int expression\n  }\t\t\t\t\t\t\t\t\n}\n"
                              ~=? False
                             ),
  

  
  TestLabel "illegal-14.dcf" (semchecker_ 
                              "class Program {\n  boolean b;\n\n  void main() {\n    b = true > false;\t\t// operands of > must be ints\n  }\t\t\t\t\t\t\t\t\n}\n"
                              ~=? False
                             ),
  
  
  
  TestLabel "illegal-15.dcf" (semchecker_ 
                              "class Program {\n  boolean b;\n\n  void main() {\n    b = 5 == true;\t\t// types of operands of == must be equal\n  }\t\t\t\t\t\t\t\t\n}\n"
                              ~=? False
                             ),
  
  
  
  TestLabel "illegal-16.dcf" (semchecker_ 
                              "class Program {\n  boolean b;\n\n  void main() {\n    b = !5;\t// operand of ! must be boolean\n  }\t\t\t\t\t\t\t\t\n}\n"
                              ~=? False
                             ),
  
  
  
  TestLabel "illegal-17.dcf" (semchecker_ 
                              "class Program {\n  boolean b;\n\n  void main() {\n    b += true;\t// lhs and rhs of += must be int\n  }\t\t\t\t\t\t\t\t\n}\n"
                              ~=? False
                             ),
  
  
  
  TestLabel "legal-01.dcf" (semchecker_ 
                            "// a quicksort program.  set the \"length\" parameter in main() to the\n// desired size of the sorted array.  if you want to sort an array\n// bigger than 100 elements, you'll also need to adjust the declaration\n// of the global array A.\n\nclass Program\n{\n    int A[100];\n    int length;\n    \n    int partition(int p, int r) \n    {\n\tint x, i, j, t;\n        int z;\n        \n\tx = A[p];\n\ti = p - 1;\n\tj = r + 1;\n\n  \tfor z = 0, length * length {\n\t  j = j - 1;\n\t    for a = 0, length {\n\t      if (A[j] <= x) {\n\t\tbreak;\n\t      }\n\t      j = j - 1;\n\t    }\n\n\t    for a = i + 1, length {\n\t      if (A[a] >= x) {\n\t\ti = a;\n\t\tbreak;\n\t      }\n\t    }\n\n   \t    if (i < j) {\n  \t\tt = A[i];\n  \t\tA[i] = A[j];\n  \t\tA[j] = t;\n  \t    } else {\n \t\treturn j;\n  \t    }\n  \t}\n\treturn -1;\n    }\n\n    void quicksort(int p, int r)\n    {\n  \tint q;\n        \n  \tif (p < r) {\n  \t    q = partition (p, r);\n  \t    quicksort (p, q);\n  \t    quicksort (q+1, r);\n  \t}\n    }\n    \n    void main() \n    {\n\tint temp;\n        \n\tlength = 10; // adjust for sort length\n        \n        callout(\"printf\", \"creating random array of %d elements\\n\", length);\n\n        callout(\"srandom\", 17);\n        \n\tfor i = 0, length {\n            temp = callout(\"random\");\n            A[i] = temp;\n        }\n        \n        callout(\"printf\", \"\\nbefore sort:\\n\");\n\tfor i = 0, length {\n   \t    callout (\"printf\", \"%d\\n\", A[i]); \n        }\n        \n        quicksort (0, length - 1);\n\n        callout(\"printf\", \"\\nafter sort\\n\");\n\tfor i = 0, length {\n\t  callout (\"printf\", \"%d\\n\", A[i]); \n  \t}\n    }\n}\n"
                            ~=? True
                           )
  ]
             
             





{-
                                
-- 1. No identifier is declared twice in the same scope.
smr_no_decl_twice = TestList [
  TestLabel "case-given-by-TA" (semchecker_ "class Program {\n void main() { \n int x; boolean x;  // identifier declared twice \n }\n}"
                                ~=? False)
  ]


-- 2. No identifier is used before it is declared.
smr_use_after_decl = TestList [
  TestLabel "case-given-by-TA" (semchecker_ "class Program { \n void main() { \n a = 34;// identifier used before being declared \n}\n}"
                                ~=? False)
  ]

-- 3. The program contains a definition for a method called main that has no parameters (note
-- that since execution starts at method main, any methods defined after main will never be
-- executed).
smr_regular_main = TestList [
  TestLabel "case-given-by-TA" (semchecker_ "// No main method \n class Program { \n void foo() { \n  } \n  void bar() { \n  } \n }"
                                ~=? False)
  ]

-- 4. The <int literal> in an array declaration must be greater than 0.
smr_pos_array = TestList [
  TestLabel "case-given-by-TA" (semchecker_ "class Program {\n int a[0];	// bad array size \n  void main() { \n } \n }\n"
                                ~=? False)
  ]

-- 5. The number and types of arguments in a method call must be the same as the number and
-- types of the formals, i.e., the signatures must be identical.
smr_call_sig_match = TestList [
  TestLabel "case-given-by-TA" (semchecker_ "class Program {\n   int foo(int i, boolean b) {\n    return 4;\n  }\n  void main() {\n    int x;\n    x = foo(34, true, true);	// argument mismatch\n  }	\n}"
                                ~=? False)
  ]

-- 6. If a method call is used as an expression, the method must return a result.
smr_call_return = TestList [
  TestLabel "case-given-by-TA" (semchecker_ "class Program {\n  int foo(int i, boolean b) {\n    return 3;\n  }\n  void main() {\n    int x;\n    x = foo(34, 35);	// types don't match signature\n  }								\n}	"
                                ~=? False)
  ]

-- 7. A return statement must not have a return value unless it appears in the body of a method
-- that is declared to return a value.
smr_redun_return = TestList [
  TestLabel "case-given-by-TA" (semchecker_ "class Program {\n  void foo(int i, boolean b) {\n    return 3;	// should not return value;\n  }\n  void main() {\n  }								\n}"
                                ~=? False)
  ]

-- 8. The expression in a return statement must have the same type as the declared result type
-- of the enclosing method definition.
smr_ret_expr_type = TestList [
  TestLabel "case-given-by-TA" (semchecker_ "class Program {\n  int foo(int i, boolean b) {\n    return true;	// return value has wrong type\n  }\n  void main() {\n    int x;\n    x = foo(34, true);\n  }							\n}"
                                ~=? False)
  ]


-- 9. An <id> used as a <location> must name a declared local/global variable or formal parameter.
smr_location_type = TestList [
  ]
                   
-- 10. For all locations of the form <id>[<expr>]
-- (a) <id> must be an array variable, and
-- (b) the type of <expr> must be int.
smr_location_type2 = TestList [
  TestLabel "case-given-by-TA09" (semchecker_ "class Program {\n  int a[10];\n  boolean b;\n  void main() {\n    a[b] = 25;	// array index has wrong type\n   }								\n}"
                                ~=? False)
  ]
                   
-- 11. The <expr> in an if statement must have type boolean.
smr_if_boolean = TestList [
  TestLabel "case-given-by-TA11" (semchecker_ "class Program {\n  int a[10];\n  void main() {\n    if (a[3]) {}	// condition should be a boolean\n  }								\n}"
                                ~=? False)
  ]
                   
-- 12. The operands of <arith op>s and hrel opis must have type int.
smr_arith_rel_int = TestList [
  TestLabel "case-given-by-TA" (semchecker_ "class Program {\n  boolean b;\n\n  void main() {\n    b = true > false;		// operands of > must be ints\n  }								\n}"
                                ~=? False)
  ]
                                                          
-- 13. The operands of <eq op>s must have the same type, either int or boolean.
smr_eqop = TestList [
  TestLabel "case-given-by-TA" (semchecker_ "class Program {\n  boolean b;\n\n  void main() {\n    b = 5 ~=? true;		// types of operands of == must be equal\n  }								\n}"
                                ~=? False)
  ]
                   
-- 14. The operands of <cond op>s and the operand of logical not (!) must have type boolean.
smr_cond_not_op = TestList [
  TestLabel "case-given-by-TA" (semchecker_ "class Program {\n  boolean b;\n  void main() {\n    b = !5;	// operand of ! must be boolean\n  }								\n}"
                                ~=? False)
  ]
                    
-- 15. The <location> and the <expr> in an assignment, <location> = <expr>, must have the same type.
smr_assign = TestList [
  TestLabel "case-given-by-TA10" (semchecker_ "class Program {\n  int a[10];\n  void main() {\n    a[1] = a;	// bad type, rhs should be an int\n  }								\n}" 
                                  ~=? False),
  
  TestLabel "case-given-by-TA13" (semchecker_ "class Program {\n  int a[10];\n  int i;\n  void main() {\n    a[3] = i < 35;	// rhs should be an int expression\n  }								\n}\n" 
                                  ~=? False)
  ]
                   
-- 16. The <location> and the <expr> in an incrementing/decrementing assignment, <location> += <expr>
-- and <location> -= <expr>, must be of type int.
smr_incdec_type = TestList [
  TestLabel "case-given-by-TA" (semchecker_ "class Program {\n  boolean b;\n\n  void main() {\n    b += true;	// lhs and rhs of += must be int\n  }								\n}"
                                ~=? False)
  ]

-- 17. The initial <expr> and the ending <expr> of for must have type int.
smr_for_tyoe = TestList [
  TestLabel "case-given-by-TA12" (semchecker_ "class Program {\n  void main() {\n    for x = false, 10 {   //initial condition must be an int\n    }\n  }							\n}"
                                ~=? False)
  ]
               
-- 18. All break and continue statements must be contained within the body of a for.
smr_break_continue = TestList [
  TestLabel "break"    (semchecker_ "class Program { \n void main() { \n break; \n }}"   ~=? False)
  TestLabel "continue" (semchecker_ "class Program { \n void main() { \n continue; \n }}"  ~=? False)
  ]


smr_misc = TestList [
  TestLabel "legal"   (semchecker_ 
                       "// a quicksort program.  set the \"length\" parameter in main() to the\n// desired size of the sorted array.  if you want to sort an array\n// bigger than 100 elements, you'll also need to adjust the declaration\n// of the global array A.\n\nclass Program\n{\n    int A[100];\n    int length;\n    \n    int partition(int p, int r) \n    {\n\tint x, i, j, t;\n        int z;\n        \n\tx = A[p];\n\ti = p - 1;\n\tj = r + 1;\n\n  \tfor z = 0, length * length {\n\t  j = j - 1;\n\t    for a = 0, length {\n\t      if (A[j] <= x) {\n\t\tbreak;\n\t      }\n\t      j = j - 1;\n\t    }\n\n\t    for a = i + 1, length {\n\t      if (A[a] >= x) {\n\t\ti = a;\n\t\tbreak;\n\t      }\n\t    }\n\n   \t    if (i < j) {\n  \t\tt = A[i];\n  \t\tA[i] = A[j];\n  \t\tA[j] = t;\n  \t    } else {\n \t\treturn j;\n  \t    }\n  \t}\n\treturn -1;\n    }\n\n    void quicksort(int p, int r)\n    {\n  \tint q;\n        \n  \tif (p < r) {\n  \t    q = partition (p, r);\n  \t    quicksort (p, q);\n  \t    quicksort (q+1, r);\n  \t}\n    }\n    \n    void main() \n    {\n\tint temp;\n        \n\tlength = 10; // adjust for sort length\n        \n        callout(\"printf\", \"creating random array of %d elements\\n\", length);\n\n        callout(\"srandom\", 17);\n        \n\tfor i = 0, length {\n            temp = callout(\"random\");\n            A[i] = temp;\n        }\n        \n        callout(\"printf\", \"\\nbefore sort:\\n\");\n\tfor i = 0, length {\n   \t    callout (\"printf\", \"%d\\n\", A[i]); \n        }\n        \n        quicksort (0, length - 1);\n\n        callout(\"printf\", \"\\nafter sort\\n\");\n\tfor i = 0, length {\n\t  callout (\"printf\", \"%d\\n\", A[i]); \n  \t}\n    }\n}\n"
                       ~=? True)

  ]



  smr_no_decl_twice,
  smr_use_after_decl,
  smr_regular_main,
  smr_call_sig_match,
  smr_pos_array,
  smr_call_return,
  smr_redun_return,
  smr_ret_expr_type,
  smr_location_type,
  smr_location_type2,
  smr_if_boolean,
  smr_arith_rel_int,
  smr_eqop,
  smr_cond_not_op,
  smr_assign,
  smr_incdec_type,
  smr_for_type, 
  smr_break_continue, 
  smr_misc,


semanticCheck :: String -> Report Bool   -- right now just pass or fail
semanticCheck input = False
  --case parse parseExpressionStream "decaf-semantic-checker" input of
  --                        Left err -> Error ("Parser Error!: " ++ show err)
  --                        Right val -> Success val




-}