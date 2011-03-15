module Decaf.IR.ASM where

class ASM a where
    -- | The 'intelasm' function formats the LIR node (pretty-print)
    -- into a gun assembly string.
    gnuasm:: a -> String

    -- | The 'intelasm' function formats the LIR node (pretty-print)
    -- into a intel assembly string.
    intelasm :: a -> String -- turn into a generic tree

gnuSuffix size = case size of
                    1 -> 'b'
                    2 -> 'w'
                    4 -> 'l'
                    8 -> 'q'

intelPrefix size = case size of
                      1 -> "byte"
                      2 -> "word"
                      4 -> "dword"
                      8 -> "qword"
