all: runtests

SRC = runtests.hs ScannerParserTests.hs SemanticsTests.hs TestRoot.hs ../Decaf/AST.hs ../Decaf/Parser.hs ../Decaf/Scanner.hs ../Decaf/Tokens.hs

OPS = -package parsec

runtests: $(SRC)
	ghc $(OPS) -o $@ $^  -v

