all: runtests

SRC =  Tokens.hs Scanner.hs AST.hs Parser.hs TestRoot.hs ScannerParserTests.hs SemanticsTests.hs runtests.hs

OPS = -package parsec

runtests: $(SRC)
	ghc $(OPS) -o $@ $^  -v

