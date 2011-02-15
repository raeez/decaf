module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Pos
import Monad
import Data.List
import Scanner


type TokenParser a = GenParser Token () a

mytoken :: (Tok -> Maybe a) -> TokenParser a
mytoken test = token showToken posToken testToken
    where
      showToken (pos,tok)   = show tok
      posToken  (pos,tok)   = pos
      testToken (pos,tok)   = test tok


{- Terminals -}

idn
    = mytoken (\tok -> case tok of 
                         Identifier name -> Just name
                         other           -> Nothing)
idname name
    = mytoken (\tok -> case tok of 
                         Identifier n | n == name -> Just ()
                         other           -> Nothing)
res name
    = mytoken (\tok -> case tok of
                         Scanner.Reserved s | s == name  -> Just ()
                         other          -> Nothing)
int
    = mytoken (\tok -> case tok of 
                         Intliteral val -> Just val
                         other -> Nothing)
char
    = mytoken (\tok -> case tok of 
                         Charliteral val -> Just val
                         other -> Nothing)
bool
    = mytoken (\tok -> case tok of 
                         Booleanliteral val -> Just val
                         other -> Nothing)
string
    = mytoken (\tok -> case tok of 
                         Stringliteral val -> Just val
                         other -> Nothing)
errortok
    = mytoken (\tok -> case tok of 
                         Error val -> Just val
                         other -> Nothing)

{- grammar/data -}

data Program = Program { fields :: [FieldDec],
                         methods :: [MethodDec] }

data FieldDec = FieldDec String
data MethodDec = MethodDec String

program = do (res "class") >> (idname "Program") >> (res "{") 
             fields <- (many fieldDec) 
             methods <- (many methodDec)
             res "}"
             return $ Program fields methods

fieldDec :: TokenParser FieldDec
fieldDec = idn >>= \s -> return $FieldDec s -- >> (many1 (id <|> (id >>

methodDec = idn >>= \s -> return $MethodDec s

varType = (res "int") <|> (res "boolean")

assignOp = (res "=") <|> (res "+=") <|> (res "-=")

methodName = idn




main :: IO ()
main = do args <- getArgs 
          str <- readFile (head args)
          tokens <- return (beginScanString str)
          str <- case runParser program () "" tokens of
            Left err  -> return "error"
            Right val -> return "success"

          putStrLn str
            

--putStrLn.(foldl (++) "").(intersperse "\n").init.(map (showToken)).beginScanString $ str