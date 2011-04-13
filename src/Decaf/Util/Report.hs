module Decaf.Util.Report where

-- | Report utilized throughout the binaries for error reporting
data Report a = RSuccess a
              | RError String

getReport :: (Show a) => Report a -> String
getReport (RSuccess a) = show a
getReport (RError s) = s

getRSuccess :: Report a -> a
getRSuccess (RSuccess a) = a
getRSuccess (RError _) = error "Util.hs:getRSuccess called on an RError object"

getRError :: Report a -> String
getRError (RError s) = s
getRError (RSuccess _) = error "Util.hs:getRError called on an RSuccess object"
