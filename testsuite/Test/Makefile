all: testScanner testParser testSemanticsChecker

SRC =  ../../Decaf/Tokens.hs ../../Decaf/AST.hs ../../Decaf/Scanner.hs ../../Decaf/Parser.hs 

OPS = -package parsec 

testScanner: testScanner.hs
	ghc $(OPS) -o $@ $(SRC) $^ -v

