sh clean
ghc -package parsec -o scanner-bin scanner.hs scanner-bin.hs
ghc -package parsec -o scanner-repl scanner.hs scanner-repl.hs
