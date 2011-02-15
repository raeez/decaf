module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Pos
import Monad
import Data.List
import Scanner

main :: IO()
main = do args <- getArgs 
          str <- readFile (head args)
          putStrLn.(foldl (++) "").(intersperse "\n").init.(map (showToken)).beginScanString $ str