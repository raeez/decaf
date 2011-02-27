--module Main
module Decaf.ScannerTests
where
import Test.HUnit
import Decaf.AST
import Decaf.Scanner
import Decaf.Parser
import Decaf.Tokens
import Text.ParserCombinators.Parsec hiding (spaces)



--main = do runTestTT scannerTests
         

scanTokens_ :: Parser [Token]
scanTokens_ = singleToken `sepEndBy` separator
              where separator = ws


scanTokens :: String -> Report [DecafToken]
scanTokens input = case parse scanTokens_ "test-scanner" input of
                      Left err -> RError ("Parser Error!: " ++ show err)
                      Right val -> RSuccess (map dToken val) 


------------------------------------------
-- scannerTests
--

scannerTests = TestList [
  numbers,
  bools,
  strings,
  characters,
  comments,
  ids,
  testsFromTA
  ]

numbers = TestList [
  TestLabel "decimal-0" ((scanTokens "0") ~=? RSuccess [DecLit "0"]),
  TestLabel "decimal-100" ((scanTokens "100") ~=? RSuccess [DecLit "100"]),
  TestLabel "hexadecimal-0" ((scanTokens "0x000") ~=? RSuccess [HexLit "0"]),
  TestLabel "hexadecimal-1" ((scanTokens "0x001") ~=? RSuccess [HexLit "1"]),
  TestLabel "hexadecimal-50" ((scanTokens "0x32") ~=? RSuccess [HexLit "50"]),
  TestLabel "dec10-hex16" ((scanTokens "10\r\r\n 0x10") ~=? RSuccess [DecLit "10", HexLit "16"]),
  TestLabel "enum 1...10" ((scanTokens "1     \t\t\t 2   \n\n\r\n   3 4 5\t6 0x7 0x8//haha, check out this comment ;-) xor'djuggajugga\n0x9//here we go again[0]@!\"!#@$!@#$!@%#$^%$^&%^&*^&*(\"\n0x1000") ~=? RSuccess [DecLit "1", DecLit "2", DecLit "3", DecLit "4", DecLit "5", DecLit "6", HexLit "7", HexLit "8", HexLit "9", HexLit "4096"])
  ]

bools = TestList [
  TestLabel "simple-bools" ((scanTokens "true false true") ~=? RSuccess[BoolLit True, BoolLit False, BoolLit True])
  ]

strings = TestList [
  TestLabel "zero-string" ((scanTokens "\"\"") ~=? RSuccess [StrLit ""]),
  TestLabel "null-string" ((scanTokens "\"\0\"") ~=? RSuccess [StrLit "\0"]),
  TestLabel "escaped-string" ((scanTokens "\"a\nbc\"") ~=? RSuccess [StrLit "a\nbc"]),
  TestLabel "spaced-string" ((scanTokens "  \t  \n\"  a   b   c  \"") ~=? RSuccess [StrLit "  a   b   c  "]),
  TestLabel "mixed-string" ((scanTokens "  \"  \\\"heLlo, I aM He\nre to sa\tve yoU!!\0!!\\\"\"") ~=? RSuccess [StrLit "  \"heLlo, I aM He\nre to sa\tve yoU!!\0!!\""])
  ]

characters = TestList [
  TestLabel "single-char" ((scanTokens "'A'") ~=? RSuccess [CharLit 'A']),
  TestLabel "badbeef-char" ((scanTokens "'b' 'a' 'd' 'b' 'e' 'e' 'f'") ~=? RSuccess [CharLit 'b', CharLit 'a', CharLit 'd', CharLit 'b', CharLit 'e', CharLit 'e', CharLit 'f']),
  TestLabel "char-string-num" ((scanTokens "'.' \"B\" 3") ~=? RSuccess [CharLit '.', StrLit "B", DecLit "3"]),
  TestLabel "chained-characters" ((scanTokens "'.''A''B'") ~=? RSuccess [])
  ]


comments = TestList [
  TestLabel "basic-comment" ((scanTokens "23//test\n24") ~=? RSuccess [DecLit "23", DecLit "24"]),
  TestLabel "complex-comment" ((scanTokens " 4// the cow, he jumped! and this happened 0x2 times, afterwards, giant[3] said \"ho,ho,ho! find for fee fum; @!#$!@#^%$^*(&*^(!@$\"\"\"\n\"test_string\"") ~=? RSuccess [DecLit "4", StrLit "test_string"])
  ]



ids = TestList [
  TestLabel "simple-vars" ((scanTokens "a Db cd e_D fgh_iJKL _g0g0_gadg3t") ~=? 
                           RSuccess [Identf "a",Identf "Db",Identf "cd",Identf "e_D",Identf "fgh_iJKL",Identf "_g0g0_gadg3t"]),
  TestLabel "non-keywords" ((scanTokens "iF If BooLeaN booLean breaK foR For VoiD InTTruE truE inT Class CLASS CONTINuE BreAK fi callouT elsE reTurN") ~=? RSuccess [Identf "iF",Identf "If",Identf "BooLeaN",Identf "booLean",Identf "breaK",Identf "foR",Identf "For",Identf "VoiD",Identf "InTTruE",Identf "truE",Identf "inT",Identf "Class",Identf "CLASS",Identf "CONTINuE",Identf "BreAK",Identf "fi",Identf "callouT",Identf "elsE",Identf "reTurN"]),
  TestLabel "keywords" ((scanTokens "break break callout boolean if if int return true void void false else class continue for") ~=? RSuccess [Reserv "break",Reserv "break",Reserv "callout",Reserv "boolean",Reserv "if",Reserv "if",Reserv "int",Reserv "return",Reserv "true",Reserv "void",Reserv "void",Reserv "false",Reserv "else",Reserv "class",Reserv "continue",Reserv "for"]),
  TestLabel "mixed" ((scanTokens "breakbreak break callout_ boolean if if int return true void void false eLse clasS continue f0r") ~=? RSuccess [Identf "breakbreak",Reserv "break",Identf "callout_",Reserv "boolean",Reserv "if",Reserv "if",Reserv "int",Reserv "return",Reserv "true",Reserv "void",Reserv "void",Reserv "false",Identf "eLse",Identf "clasS",Reserv "continue",Identf "f0r"]),
  TestLabel "invalid-!" ((scanTokens "break!") ~=? RError ""),
  TestLabel "invalid-3" ((scanTokens "3l3t3") ~=? RError "")  
               ]










testsFromTA = TestList [
  TestLabel "char1" (scannerShow 
"// Mundane characters.\n'a' 'b' 'c'\n'R' 'i' 'n' 'a' 'r' 'd'\n'6' '0' '3' '5'\n"
                                           ~=?
"[L2:C1-4] CHARLITERAL 'a'\n[L2:C5-8] CHARLITERAL 'b'\n[L2:C9-12] CHARLITERAL 'c'\n[L3:C1-4] CHARLITERAL 'R'\n[L3:C5-8] CHARLITERAL 'i'\n[L3:C9-12] CHARLITERAL 'n'\n[L3:C13-16] CHARLITERAL 'a'\n[L3:C17-20] CHARLITERAL 'r'\n[L3:C21-24] CHARLITERAL 'd'\n[L4:C1-4] CHARLITERAL '6'\n[L4:C5-8] CHARLITERAL '0'\n[L4:C9-12] CHARLITERAL '3'\n[L4:C13-16] CHARLITERAL '5'\n\n"
                                            ),



  TestLabel "char2" (scannerShow 
"// Backslashed characters.\n'\\n' '\\t' '\\\\' '\\\"'\n"
                                           ~=?
"[L2:C1-5] CHARLITERAL '\\n'\n[L2:C6-10] CHARLITERAL '\\t'\n[L2:C11-15] CHARLITERAL '\\\\'\n[L2:C16-20] CHARLITERAL '\\\"'\n\n"
                                            ),



  TestLabel "char3" (scannerShow 
"// Mismatched single quotes.\n'''\n"
                                           ~=?
"Decaf Compiler\nFile // Mismatched single quotes.\n'''\n has lex errors!\nToken Stream:\n\n\n[L2:C2-4] SCANNER ERROR: \"decaf-scanner-eatFirst\" (line 2, column 2):\nunexpected \"'\"\nexpecting quoted character\n[L2:C4-6] SCANNER ERROR: \"decaf-scanner-eatNext\" (line 2, column 4):\nunexpected \"\\n\"\nexpecting quoted character\n\n"
                                            ),



  TestLabel "char4" (scannerShow 
"// Invalid backslashed character.\n'\\p'\n"
                                           ~=?
"Decaf Compiler\nFile // Invalid backslashed character.\n'\\p'\n has lex errors!\nToken Stream:\n\n\n[L2:C3-5] SCANNER ERROR: \"decaf-scanner-eatFirst\" (line 2, column 3):\nunexpected \"p\"\nexpecting \"'\"\n[L2:C5-7] SCANNER ERROR: \"decaf-scanner-eatNext\" (line 2, column 5):\nunexpected \"\\n\"\nexpecting quoted character\n\n"
                                            ),



  TestLabel "char5" (scannerShow 
"// newline isn't a valid char.\n'\n'\n"
                                           ~=?
"Decaf Compiler\nFile // newline isn't a valid char.\n'\n'\n has lex errors!\nToken Stream:\n\n\n[L2:C2-4] SCANNER ERROR: \"decaf-scanner-eatFirst\" (line 2, column 2):\nunexpected \"\\n\"\nexpecting quoted character\n[L3:C2-4] SCANNER ERROR: \"decaf-scanner-eatNext\" (line 3, column 2):\nunexpected \"\\n\"\nexpecting quoted character\n\n"
                                            ),



  TestLabel "char6" (scannerShow 
"// Double quotes need to be escaped.\n'\"'\n"
                                           ~=?
"Decaf Compiler\nFile // Double quotes need to be escaped.\n'\"'\n has lex errors!\nToken Stream:\n\n\n[L2:C2-4] SCANNER ERROR: \"decaf-scanner-eatFirst\" (line 2, column 2):\nunexpected \"\\\"\"\nexpecting quoted character\n[L2:C4-6] SCANNER ERROR: \"decaf-scanner-eatNext\" (line 2, column 4):\nunexpected \"\\n\"\nexpecting quoted character\n\n"
                                            ),



  TestLabel "char7" (scannerShow 
"// The backslash quotes the single quote, and the char literal doesn't end.\n'\\'\n"
                                           ~=?
"Decaf Compiler\nFile // The backslash quotes the single quote, and the char literal doesn't end.\n'\\'\n has lex errors!\nToken Stream:\n\n\n[L2:C4-6] SCANNER ERROR: \"decaf-scanner-eatFirst\" (line 2, column 4):\nunexpected \"\\n\"\nexpecting \"'\"\n\n"
                                            ),



  TestLabel "char8" (scannerShow 
"// Char literals can only have one char.\n'ab'\n"
                                           ~=?
"Decaf Compiler\nFile // Char literals can only have one char.\n'ab'\n has lex errors!\nToken Stream:\n\n\n[L2:C3-5] SCANNER ERROR: \"decaf-scanner-eatFirst\" (line 2, column 3):\nunexpected \"b\"\nexpecting \"'\"\n[L2:C5-7] SCANNER ERROR: \"decaf-scanner-eatNext\" (line 2, column 5):\nunexpected \"\\n\"\nexpecting quoted character\n\n"
                                            ),



  TestLabel "char9" (scannerShow 
"// Tab inside a quoted string.\n'\t'\n"
                                           ~=?
"[L2:C1-4] CHARLITERAL ' '\n\n"
                                            ),



  TestLabel "hexlit1" (scannerShow 
"// Basic hexadecimal literals.\n0x0\n0x1\n0xe43620\n0x11\n0xbeef\n0xF\n0xF00\n0xB1ad\n"
                                           ~=?
"[L2:C1-4] INTLITERAL 0x0\n[L3:C1-4] INTLITERAL 0x1\n[L4:C1-9] INTLITERAL 0xe43620\n[L5:C1-5] INTLITERAL 0x11\n[L6:C1-7] INTLITERAL 0xbeef\n[L7:C1-4] INTLITERAL 0xF\n[L8:C1-6] INTLITERAL 0xF00\n[L9:C1-7] INTLITERAL 0xB1ad\n\n"
                                            ),



  TestLabel "hexlit2" (scannerShow 
"// This is way too big, but we're not checking this in the scanner.\n0xDEADBEEF\n"
                                           ~=?
"[L2:C1-11] INTLITERAL 0xDEADBEEF\n\n"
                                            ),



  TestLabel "hexlit3" (scannerShow 
"// This isn't a hex literal, since there are no digits.\n0x\n"
                                           ~=?
"Decaf Compiler\nFile // This isn't a hex literal, since there are no digits.\n0x\n has lex errors!\nToken Stream:\n\n\n[L2:C3-5] SCANNER ERROR: \"decaf-scanner-eatFirst\" (line 2, column 3):\nunexpected \"\\n\"\nexpecting hexadecimal digit\n\n"
                                            ),



  TestLabel "id1" (scannerShow 
"// Some valid identifiers of various sorts.\nabcdefg\nRinard\nmartin_rinard\nsix_dot_035\n_foo_\n\n"
                                           ~=?
"[L2:C1-8] IDENTIFIER abcdefg\n[L3:C1-7] IDENTIFIER Rinard\n[L4:C1-14] IDENTIFIER martin_rinard\n[L5:C1-12] IDENTIFIER six_dot_035\n[L6:C1-6] IDENTIFIER _foo_\n\n"
                                            ),



  TestLabel "id2" (scannerShow 
"// This should not be a single identifier\nfoo.bar\n\n"
                                           ~=?
"Decaf Compiler\nFile // This should not be a single identifier\nfoo.bar\n\n has lex errors!\nToken Stream:\n\n\n[L2:C1-4] IDENTIFIER foo\n[L2:C4-6] SCANNER ERROR: \"decaf-scanner-eatNext\" (line 2, column 4):\nunexpected \".\"\nexpecting whitespace, operator, literal, identifier, \";\", \"[\", \"]\", \"(\", \")\", \"{\", \"}\", \",\" or end of input\n[L2:C5-8] IDENTIFIER bar\n\n"
                                            ),



  TestLabel "id3" (scannerShow 
"// Things with different cases\nif\niF\nIf\nfoo Foo\nNew\n\n"
                                           ~=?
"[L2:C1-3] if\n[L3:C1-3] IDENTIFIER iF\n[L4:C1-3] IDENTIFIER If\n[L5:C1-4] IDENTIFIER foo\n[L5:C5-8] IDENTIFIER Foo\n[L6:C1-4] IDENTIFIER New\n\n"
                                            ),



  TestLabel "number1" (scannerShow 
"// This number is too large, but we don't check in the scanner.  \n-2147483649\n"
                                           ~=?
"[L2:C1-2] -\n[L2:C2-12] INTLITERAL 2147483649\n\n"
                                            ),



  TestLabel "number2" (scannerShow 
"// Some perfectly normal mundane numbers.\n0\n1\n-1\n259\n17\n43 -620\n"
                                           ~=?
"[L2:C1-2] INTLITERAL 0\n[L3:C1-2] INTLITERAL 1\n[L4:C1-2] -\n[L4:C2-3] INTLITERAL 1\n[L5:C1-4] INTLITERAL 259\n[L6:C1-3] INTLITERAL 17\n[L7:C1-3] INTLITERAL 43\n[L7:C4-5] -\n[L7:C5-8] INTLITERAL 620\n\n"
                                            ),



  TestLabel "op1" (scannerShow 
"// Some operators.\n+ - * < <= != &&\n"
                                           ~=?
"[L2:C1-2] +\n[L2:C3-4] -\n[L2:C5-6] *\n[L2:C7-8] <\n[L2:C9-11] <=\n[L2:C12-14] !=\n[L2:C15-17] &&\n\n"
                                            ),



  TestLabel "op2" (scannerShow 
"// ++ is two tokens, so these two lines are equivalent\na++\na+ +"
                                           ~=?
"[L2:C1-2] IDENTIFIER a\n[L2:C2-3] +\n[L2:C3-4] +\n[L3:C1-2] IDENTIFIER a\n[L3:C2-3] +\n[L3:C4-5] +\n\n"
                                            ),



  TestLabel "string1" (scannerShow 
"// Simple strings.  \n\"A string walks into a bar and orders a beer.\"\n\"The bartender looks at him and says, \\\"we don\\'t serve strings here.\\\"\"\n\"The string walks out to the street, and sits on the curb, dejected.\"\n\"Then he has an idea: he ties himself into a bow, and loosens up his\"\n\"ends, making them up into nice tassels.\"\n\"His confidence restored, he walks back into the bar, sits down, and orders\"\n\"another beer.\"\n\"The bartender looks at him suspiciously: he looks a bit like the string\"\n\"that had just walked in.  \\\"Hey,\\\" he says, \\\"aren\\'t you a string?\\\"\"\n\"\\\"Nope,\\\" says the string.  \\\"I\\'m a frayed knot.\\\"\"\n"
                                           ~=?
"[L2:C1-47] STRINGLITERAL \"A string walks into a bar and orders a beer.\"\n[L3:C1-73] STRINGLITERAL \"The bartender looks at him and says, \\\"we don\\'t serve strings here.\\\"\"\n[L4:C1-70] STRINGLITERAL \"The string walks out to the street, and sits on the curb, dejected.\"\n[L5:C1-70] STRINGLITERAL \"Then he has an idea: he ties himself into a bow, and loosens up his\"\n[L6:C1-42] STRINGLITERAL \"ends, making them up into nice tassels.\"\n[L7:C1-77] STRINGLITERAL \"His confidence restored, he walks back into the bar, sits down, and orders\"\n[L8:C1-16] STRINGLITERAL \"another beer.\"\n[L9:C1-74] STRINGLITERAL \"The bartender looks at him suspiciously: he looks a bit like the string\"\n[L10:C1-72] STRINGLITERAL \"that had just walked in.  \\\"Hey,\\\" he says, \\\"aren\\'t you a string?\\\"\"\n[L11:C1-54] STRINGLITERAL \"\\\"Nope,\\\" says the string.  \\\"I\\'m a frayed knot.\\\"\"\n\n"
                                            ),



  TestLabel "string2" (scannerShow 
"// An unquoted single quote.\n\"Aren't you a string?\"\n"
                                           ~=?
"Decaf Compiler\nFile // An unquoted single quote.\n\"Aren't you a string?\"\n has lex errors!\nToken Stream:\n\n\n[L2:C6-8] SCANNER ERROR: \"decaf-scanner-eatFirst\" (line 2, column 6):\nunexpected \"'\"\nexpecting quoted character or \"\\\"\"\n[L2:C7-8] IDENTIFIER t\n[L2:C9-12] IDENTIFIER you\n[L2:C13-14] IDENTIFIER a\n[L2:C15-21] IDENTIFIER string\n[L2:C21-23] SCANNER ERROR: \"decaf-scanner-eatNext\" (line 2, column 21):\nunexpected \"?\"\nexpecting whitespace, operator, literal, identifier, \";\", \"[\", \"]\", \"(\", \")\", \"{\", \"}\", \",\" or end of input\n[L2:C23-25] SCANNER ERROR: \"decaf-scanner-eatNext\" (line 2, column 23):\nunexpected \"\\n\"\nexpecting quoted character or \"\\\"\"\n\n"
                                            ),



  TestLabel "string3" (scannerShow 
"// A string with a comment at the end\n\"I\\\" \\\\\\\"\\'\\'\\t\\tam a STRING\\n\"//comment\n"
                                           ~=?
"[L2:C1-32] STRINGLITERAL \"I\\\" \\\\\\\"\\'\\'\\t\\tam a STRING\\n\"\n\n"
                                            ),



  TestLabel "tokens1" (scannerShow 
"// Decaf keywords\nboolean\ncallout\nclass\nelse\nfalse\nif\nint\nreturn\ntrue\nvoid\nfor\nforpar\nbreak\ncontinue\n"
                                           ~=?
"[L2:C1-8] boolean\n[L3:C1-8] callout\n[L4:C1-6] class\n[L5:C1-5] else\n[L6:C1-6] BOOLEANLITERAL false\n[L7:C1-3] if\n[L8:C1-4] int\n[L9:C1-7] return\n[L10:C1-5] BOOLEANLITERAL true\n[L11:C1-5] void\n[L12:C1-4] for\n[L13:C1-7] IDENTIFIER forpar\n[L14:C1-6] break\n[L15:C1-9] continue\n\n"
                                            ),



  TestLabel "tokens2" (scannerShow 
"// Decaf keywords in uppercase.  This should be identifiers.\nBOOLEAN\nCALLOUT\nCLASS\nELSE\nFALSE\nIF\nINT\nRETURN\nTRUE\nVOID\nFOR\nFORPAR\nBREAK\nCONTINUE\n"
                                           ~=?
"[L2:C1-8] IDENTIFIER BOOLEAN\n[L3:C1-8] IDENTIFIER CALLOUT\n[L4:C1-6] IDENTIFIER CLASS\n[L5:C1-5] IDENTIFIER ELSE\n[L6:C1-6] IDENTIFIER FALSE\n[L7:C1-3] IDENTIFIER IF\n[L8:C1-4] IDENTIFIER INT\n[L9:C1-7] IDENTIFIER RETURN\n[L10:C1-5] IDENTIFIER TRUE\n[L11:C1-5] IDENTIFIER VOID\n[L12:C1-4] IDENTIFIER FOR\n[L13:C1-7] IDENTIFIER FORPAR\n[L14:C1-6] IDENTIFIER BREAK\n[L15:C1-9] IDENTIFIER CONTINUE\n\n"
                                            ),



  TestLabel "tokens3" (scannerShow 
"// Decaf keywords stuck together.  This should be one big identifier.\nbooleancalloutclasselsefalseifintreturntruevoidforforparbreakcontinue\n\n"
                                           ~=?
"[L2:C1-70] IDENTIFIER booleancalloutclasselsefalseifintreturntruevoidforforparbreakcontinue\n\n"
                                            ),



  TestLabel "tokens4" (scannerShow 
"// Random tokens\n{-123-a35,id3a,+*;}[||===!=()&&]<><=>==\na[24]='7'; n!=if;\nfalse,-if;true32;\nforpar\n\n"
                                           ~=?
"[L2:C1-2] {\n[L2:C2-3] -\n[L2:C3-6] INTLITERAL 123\n[L2:C6-7] -\n[L2:C7-10] IDENTIFIER a35\n[L2:C10-11] ,\n[L2:C11-15] IDENTIFIER id3a\n[L2:C15-16] ,\n[L2:C16-17] +\n[L2:C17-18] *\n[L2:C18-19] ;\n[L2:C19-20] }\n[L2:C20-21] [\n[L2:C21-23] ||\n[L2:C23-25] ==\n[L2:C25-26] =\n[L2:C26-28] !=\n[L2:C28-29] (\n[L2:C29-30] )\n[L2:C30-32] &&\n[L2:C32-33] ]\n[L2:C33-34] <\n[L2:C34-35] >\n[L2:C35-37] <=\n[L2:C37-39] >=\n[L2:C39-40] =\n[L3:C1-2] IDENTIFIER a\n[L3:C2-3] [\n[L3:C3-5] INTLITERAL 24\n[L3:C5-6] ]\n[L3:C6-7] =\n[L3:C7-10] CHARLITERAL '7'\n[L3:C10-11] ;\n[L3:C12-13] IDENTIFIER n\n[L3:C13-15] !=\n[L3:C15-17] if\n[L3:C17-18] ;\n[L4:C1-6] BOOLEANLITERAL false\n[L4:C6-7] ,\n[L4:C7-8] -\n[L4:C8-10] if\n[L4:C10-11] ;\n[L4:C11-17] IDENTIFIER true32\n[L4:C17-18] ;\n[L5:C1-7] IDENTIFIER forpar\n\n"
                                            ),



  TestLabel "ws1" (scannerShow 
"// White-space characters.  This should produce several identifiers.\nfoo bar\nbaz\tquux\nmeep  \t\t\t\t  peem\nwhaah\fboom\ndoom\n\n\n\f\t\t\f\ngloom    loom\tweave\n"
                                           ~=?
"[L2:C1-4] IDENTIFIER foo\n[L2:C5-8] IDENTIFIER bar\n[L3:C1-4] IDENTIFIER baz\n[L3:C5-9] IDENTIFIER quux\n[L4:C1-5] IDENTIFIER meep\n[L4:C13-17] IDENTIFIER peem\n[L5:C1-6] IDENTIFIER whaah\n[L5:C7-11] IDENTIFIER boom\n[L6:C1-5] IDENTIFIER doom\n[L10:C1-6] IDENTIFIER gloom\n[L10:C10-14] IDENTIFIER loom\n[L10:C15-20] IDENTIFIER weave\n\n"
                                            )
  ]