module Scanner
where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import Text.ParserCombinators.Parsec.Error

type Token = (SourcePos, DecafToken)

instance Show DecafToken where
  show (StrLit s) = "STRINGLITERAL \"" ++ s ++ "\""
  show (CharLit s) = "CHARLITERAL '" ++ show s ++ "'"
  show (DecLit s) = "INTLITERAL " ++ s
  show (HexLit s) = "INTLITERAL 0x" ++ s
  show (BoolLit s) = "BOOLEANLITERAL " ++ show s
  show (Identf s) = "IDENTIFIER " ++ s
  show (Reserv s) = "RIDENTIFIER " ++ s
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
  show (Fail s) = "PARSE_ERROR: " ++ s

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

repl s = crepl readTokens s
crepl c s =  putStrLn $ unlines $ map showToken $ getReport $ c s

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

scanner :: String -> ([Token], [ParseError])
scanner input = scanIter input input [] []

scanIter :: String -> String -> [Token] -> [ParseError] -> ([Token], [ParseError])
scanIter input input' tokens errors = case parse tokenStream "decaf-scanner" input' of
                                             Left err ->  scanIter input (remainingInput $ errorPos err) tokens (errors ++ [err])
                                             Right val -> (tokens ++ val, errors)
                                             where
                                               remainingInput errPos = skipTo input $ incSourceColumn errPos 1

skipTo :: String -> SourcePos -> String
skipTo input p = case (sourceLine p) == 0 of
                   False -> skipTo ((unlines . tail . lines) input) (incSourceLine p 1)
                   otherwise -> case (sourceColumn p) == 0 of
                                  False -> skipTo (tail input) (incSourceColumn p 1)
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
firstToken = end <|> singleToken <|> (ws >> singleToken)

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
                char '\''
                p <- getPosition
                c <- anyChar
                char '\''
                return $ (p, CharLit c)

strLiteral :: Parser Token
strLiteral = do
              char '"'
              p <- getPosition
              s <- many (quoted)
              char '"'
              return $ (p, StrLit s)
              where
                quoted = try (char '\\' >> oneOf "\'\"\t\n" >>= return)
                  <|> noneOf "\""

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
