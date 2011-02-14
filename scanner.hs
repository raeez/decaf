module Scanner
where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import Text.ParserCombinators.Parsec.Error
import Data.List

type Token = (SourcePos, DecafToken)

strShow (c:cs) | c == '\t' = ('\\' : 't' : strShow cs)
             | c == '\n' = ('\\' : 'n' : strShow cs)
             | c == '\"' = ('\\' : '"' : strShow cs)
             | c == '\'' = ('\\' : '\'' : strShow cs)
             | c == '\\' = ('\\' : '\\' : strShow cs)
             | otherwise = (c:strShow cs)
strShow [] = []

instance Show DecafToken where
  show (StrLit s) = "STRINGLITERAL \"" ++ strShow s ++ "\""
  show (CharLit s) | s == '"' = "CHARLITERAL '\\\"'"
                   | otherwise  = "CHARLITERAL " ++ show s
  show (DecLit s) = "INTLITERAL " ++ s
  show (HexLit s) = "INTLITERAL 0x" ++ s
  show (BoolLit s) | s == False = "BOOLEANLITERAL false"
                   | s == True = "BOOLEANLITERAL true"
  show (Identf s) = "IDENTIFIER " ++ s
  show (Reserv s) = s
  show (LParen) = "("
  show (RParen) = ")"
  show (RBrace) = "}"
  show (LBrace) = "{"
  show (LBrack) = "["
  show (RBrack) = "]"
  show (Semi) = ";"
  show (OpAnd) = "&&"
  show (OpOr) = "||"
  show (Assign) = "="
  show (PlusAssign) = "+="
  show (MinusAssign) = "-="
  show (Comma) = ","
  show (Not) = "!"
  show (OpEq) = "=="
  show (OpNEq) = "!="
  show (OpLT) = "<"
  show (OpGT) = ">"
  show (OpLTE) = "<="
  show (OpGTE) = ">="
  show (OpAdd) = "+"
  show (OpMin) = "-"
  show (OpMul) = "*"
  show (OpDiv) = "/"
  show (OpMod) = "%"
  show (EOF) = "EOF"
  show (Fail s) = "SCANNER ERROR: " ++ s

data Report a = Success a
              | Error ParseError
              deriving (Show)

showToken :: (SourcePos, DecafToken) -> String
showToken (p, t) = (show . sourceLine $ p) ++ " " ++ (show t)

getReport (Success a) =  a
getReport (Error e) = [(errorPos e, Fail $ show e)]

data DecafToken = Fail String
                | Identf String
                | Reserv String
                | StrLit String
                | HexLit String
                | DecLit String
                | CharLit Char
                | BoolLit Bool
                | LParen
                | RParen
                | LBrace
                | RBrace
                | LBrack
                | RBrack
                | Comma
                | Semi
                | OpAnd
                | OpOr
                | OpEq
                | OpNEq
                | OpLT
                | OpGT
                | OpLTE
                | OpGTE
                | OpAdd
                | OpMin
                | OpMul
                | OpDiv
                | OpMod
                | Assign
                | PlusAssign
                | MinusAssign
                | Not
                | EOF
                deriving (Eq)

--------------------------------------------
-- helper func
--------------------------------------------
repl s = createREPL readTokens s -- a repl, for use with ghci
createREPL c s =  putStrLn $ unlines $ map showToken $ getReport $ c s

readTokens input = case parse tokenStream "test-scanner" input of
                          Left err -> Error err
                          Right val -> Success val

run parser input = case parse parser "test-parser" input of
                        Left err -> (putStrLn . show) err
                        Right val -> (putStrLn . show) val
--------------------------------------------
-- scanner
--------------------------------------------
--

scprint = putStrLn . formattedOutput . scanner
formattedOutput scannerOutput = unlines $ map showToken $ getMixedTokenStream scannerOutput
                                where
                                  getMixedTokenStream (a,b,c) = c

scanner :: String -> ([Token], [ParseError], [Token])
scanner input = scanIter input input [] [] []

scanIter :: String -> String -> [Token] -> [ParseError] -> [Token] -> ([Token], [ParseError], [Token])
scanIter input input' tokens errors both = case parse tokenStream "decaf-scanner" input' of
                                             Left err ->  scanIter input (remainingInput $ errorPos err) tokens (errors ++ [err]) (both ++ [(errorPos err, Fail $ show err)])
                                             Right val -> (tokens ++ val, errors, both ++ val)
                                             where
                                               remainingInput ep = skipTo input $ incSourceColumn ep 1

wrapIter2 input = scanIter2 input input [] [] []
scanIter2 input input' tokens errors both = case parse tokenStream "decaf-scanner" input' of
                                             Left err ->  Left $ (remainingInput (errorPos err), (errorPos err))
                                             Right val -> Right (tokens ++ val, errors, both ++ val)
                                             where
                                               remainingInput ep = skipTo input $ incSourceColumn ep 1

skipTo :: String -> SourcePos -> String
skipTo "" _ = ""
skipTo input p = case (sourceLine p) <= 1 of
                      False -> skipTo (init $ unlines $ tail $ lines input) (incSourceLine p (-1))
                      otherwise -> case (sourceColumn p) <= 1 of
                                     False -> skipTo (tail input) (incSourceColumn p (-1))
                                     otherwise -> input

tokenStream :: Parser [Token]
tokenStream = do
                first <- firstToken
                ws
                rest <- many nToken
                e <- end
                return $ [first] ++ rest ++ [e]

nToken :: Parser Token
nToken = do
          t <- singleToken
          ws
          return t

firstToken :: Parser Token
firstToken = (ws >> singleToken) <|> singleToken

end :: Parser Token
end = do
        p <- getPosition
        eof
        return (p, EOF)

singleToken :: Parser Token
singleToken = operator <|> literal <|> identifier <|> term

--------------------------------------------
-- terminals
--------------------------------------------
--
term :: Parser Token
term = do
          p <- getPosition
          b <- char ';' <|> char '[' <|> char ']' <|> char '(' <|> char ')' <|> char '{' <|> char '}' <|> char ','
          return (p, mapTerm b)
          where
            mapTerm b | b == ';' = Semi
                      | b == '[' = LBrack
                       | b == ']' = RBrack
                       | b == '(' = LParen
                       | b == ')' = RParen
                       | b == '{' = LBrace
                       | b == '}' = RBrace
                       | b == ',' = Comma
--------------------------------------------
-- operators
--------------------------------------------
--

operator :: Parser Token
operator = notOp
        <|> eqOp
        <|> condOp
        <|> relOp
        <|> arithOp

notOp :: Parser Token
notOp = do
          p <- getPosition
          (try (char '!' >> char '=') >> return (p, OpNEq)) <|> (char '!' >> return (p, Not))

eqOp :: Parser Token
eqOp = do
        p <- getPosition
        (try (char '=' >> char '=') >> return (p, OpEq)) <|> (char '=' >> return (p, Assign))

condOp :: Parser Token
condOp = do
          p <- getPosition
          (string "&&" >> return (p, OpAnd)) <|> (string "||" >> return (p, OpOr))

arithOp :: Parser Token
arithOp = plusOp
       <|> minOp
       <|> unaryOp
             
unaryOp :: Parser Token
unaryOp = do
            p <- getPosition
            c <- oneOf "+-*/%"
            return (p, mapOp(c))
            where
              mapOp c | c == '+' = OpAdd
                      | c == '-' = OpMin
                      | c == '*' = OpMul
                      | c == '/' = OpDiv
                      | c == '%' = OpMod

plusOp :: Parser Token
plusOp = do
          p <- getPosition
          (try (char '+' >> char '=') >> return (p, PlusAssign)) <|> (char '+' >> return (p, OpAdd))

minOp :: Parser Token
minOp = do
          p <- getPosition
          (try (char '-' >> char '=') >> return (p, MinusAssign)) <|> (char '-' >> return (p, OpMin))

relOp :: Parser Token
relOp = do
          p <- getPosition
          (try (char '<' >> char '=') >> return (p, OpLTE)) <|> (try (char '>' >> char '=') >> return (p, OpGTE)) <|> (char '<' >> return (p, OpLT)) <|> (char '>' >> return (p, OpGT))
            
--------------------------------------------
-- identifiers
--------------------------------------------
--

makeIdentifier :: String -> DecafToken
makeIdentifier (t) = case (t `elem` keywords) of
                                  False -> Identf t
                                  otherwise -> case (t `elem` ["true", "false"]) of
                                                False -> Reserv t
                                                otherwise -> case (t == "true") of
                                                              True -> BoolLit True
                                                              False -> BoolLit False
                      where
                        keywords = [
                          "boolean",
                          "break",
                          "callout",
                          "class",
                          "continue",
                          "else",
                          "false",
                          "for",
                          "if",
                          "int",
                          "return",
                          "true",
                          "void"
                          ]
                        

identifier :: Parser Token
identifier = do
                p <- getPosition
                h <- letter <|> char '_'
                r <- many (letter <|> digit <|> char '_')
                let str = [h] ++ r
                return $ (p, makeIdentifier(str))

--------------------------------------------
-- literals
--------------------------------------------
--
literal :: Parser Token
literal = chrLiteral
       <|> strLiteral
       <|> numLiteral

chrLiteral :: Parser Token
chrLiteral = do
                p <- getPosition
                char '\''
                c <- chrQuoted
                char '\''
                return $ (p, CharLit c)

strLiteral :: Parser Token
strLiteral = do
              char '"'
              p <- getPosition
              s <- many (strQuoted)
              char '"'
              return $ (p, StrLit s)

chrQuoted = try (char '\\' >> ((oneOf "\\\'\"" >>= return) <|> (char 'n' >> return '\n') <|> (char 't' >> return '\t'))) <|> noneOf "\"\'\t\n" <?> "quoted character"

strQuoted = try (char '\\' >> ((oneOf "\\\'\"" >>= return) <|> (char 'n' >> return '\n') <|> (char 't' >> return '\t'))) <|> noneOf "\"\'\t\n" <?> "quoted character"

numLiteral :: Parser Token
numLiteral = do
                do
                  p <- getPosition
                  try $ string "0x"
                  d <- many1 (oneOf "abcdefABCDEF0123456789")
                  return $ (p, HexLit d)
                <|> do
                      p <- getPosition
                      d <- many1 digit
                      return $ (p, DecLit d)

--------------------------------------------
-- whitespace
--------------------------------------------
--
comment :: Parser ()
comment = do
            string "//"
            skipMany (noneOf "\r\n")
            eol
            return ()

eol :: Parser ()
eol = do
        try (string "\n\r") <|> try (string "\r\n") <|> string "\n" <|> string "\r"
        return ()

sp :: Parser ()
sp = do
       char '\t'<|> char ' '
       return ()

whitespace :: Parser ()
whitespace = do
              sp <|> comment <|> eol

ws :: Parser ()
ws = skipMany whitespace
