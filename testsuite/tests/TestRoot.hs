module TestRoot
where
import Test.HUnit


data Report a = Success a
              | Error String
              deriving (Show, Eq)


