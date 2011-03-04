module Decaf.Util where

-- | Report utilized throughout the binaries for error reporting
data Report a = RSuccess a
              | RError String

getReport :: (Show a) => Report a -> String
getReport (RSuccess a) = show a
getReport (RError s) = s
