#!/bin/bash

export GHC="runghc -i../src -i../Decaf"

for T in \
  Test/testScanner.hs \
  Test/testParser.hs \
  Test/testSemanticsChecker.hs \
  ;
do echo "-- $T --" && $GHC -package parsec $T ../Decaf/Tokens.hs;
done
