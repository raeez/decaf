module Scanner
where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import Text.ParserCombinators.Parsec.Error
import Text.Regex.Posix
import Data.List

type Token = (SourcePos, SourcePos, DecafToken)

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
  show (Fail s) = "SCANNER ERROR: " ++ s
  show (EOF) = "EOF"

data Report a = Success a
              | Error ParseError
              deriving (Show)

showToken :: (SourcePos, SourcePos, DecafToken) -> String
showToken (p1, p2, t) = (show . sourceLine $ p1) ++ " " ++ (show t)

endPos :: (SourcePos, SourcePos, DecafToken) -> SourcePos
endPos (p1, p2, t) = p2

dToken :: (SourcePos, SourcePos, DecafToken) -> DecafToken
dToken (p1, p2, t) = t

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
repl s = createREPL eatFirst s -- a repl, for use with ghci
createREPL c s =  putStrLn $ unlines $ map showToken $ c s

scanner = eatFirst
--------------------------------------------
-- scanner
--------------------------------------------
--
scprint = putStrLn . formattedOutput . eatFirst
formattedOutput scannerOutput = unlines $ map showToken $ scannerOutput

eatNext parser input = case parse parser "decaf-scanner-eatNext" input of
                        Left err -> [(errorPos err, errorPos err, Fail $ show err)] ++ eatNext(parser' err) input
                        Right val -> case (dToken val == EOF) of
                                      False -> [val] ++ (eatNext (parser'' val) input)
                                      otherwise -> [val]
                        where
                          parser' e = (eatPos input $ incSourceColumn (errorPos e) $ beforeOrAfter e) >> (singleToken <|> end)
                          parser'' e = (eatPos input $ endPos e) >> ws >> (singleToken <|> end)

eatFirst input = case parse (firstToken) "decaf-scanner-eatFirst" input of
                  Left err -> [(errorPos err, errorPos err, Fail $ show err)] ++ eatNext(parser' err) input
                  Right val -> case (dToken val == EOF) of
                                False -> [val] ++ (eatNext (parser'' val) input)
                                otherwise -> [val]
                  where
                    parser' e = (eatPos input $ incSourceColumn (errorPos e) $ beforeOrAfter e) >> (singleToken <|> end)
                    parser'' e = (eatPos input $ endPos e) >> ws >> (singleToken <|> end)

beforeOrAfter e = case show e =~ "unexpected end of input" of
                    False -> 1
                    otherwise -> 0

eatPos :: String -> SourcePos -> Parser ()
eatPos input pos = eatN $ posCount input pos

posCount :: String -> SourcePos -> Int
posCount input p = let line = head $ lines input in
                     case (sourceLine p) == 1 of
                       False -> (length line) + 1 + (posCount (unlines $ tail $ lines input) (incSourceLine p (-1)))
                       otherwise -> case (sourceColumn p) == 1 of
                         False -> 1 + (posCount (tail input) (incSourceColumn p (-1)))
                         otherwise -> 0

eatN :: Int -> Parser ()
eatN 0 = (return ())
eatN n = anyChar >> (eatN (n-1))

firstToken :: Parser Token
firstToken = (ws >> (singleToken <|> end)) <|> singleToken <|> end

singleToken :: Parser Token
singleToken = do
                ws
                t <- (operator <|> literal <|> identifier <|> term)
                ws
                return t


end :: Parser Token
end = do
        p1 <- getPosition
        eof
        p2 <- getPosition
        return (p1, p2, EOF)


--------------------------------------------
-- terminals
--------------------------------------------
--
term :: Parser Token
term = do
          p1 <- getPosition
          b <- char ';' <|> char '[' <|> char ']' <|> char '(' <|> char ')' <|> char '{' <|> char '}' <|> char ','
          p2 <- getPosition
          return (p1, p2, mapTerm b)
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
          p1 <- getPosition
          o <- (try $ string "!=") <|> string "!"
          p2 <- getPosition
          return (p1, p2, mapOp o)
          where
            mapOp o | o == "!=" = OpNEq
                    | o == "!" = Not

eqOp :: Parser Token
eqOp = do
        p1 <- getPosition
        o <- (try $ string "==") <|> (string "=")
        p2 <- getPosition
        return (p1, p2, mapOp o)
        where
          mapOp o | o == "==" = OpEq
                  | o == "=" = Assign

condOp :: Parser Token
condOp = do
          p1 <- getPosition
          o <- (string "&&") <|> (string "||")
          p2 <- getPosition
          return (p1, p2, mapOp o)
          where
            mapOp o | o == "&&" = OpAnd
                    | o == "||" = OpOr

arithOp :: Parser Token
arithOp = plusOp
       <|> minOp
       <|> unaryOp
             
unaryOp :: Parser Token
unaryOp = do
            p1 <- getPosition
            c <- oneOf "+-*/%"
            p2 <- getPosition
            return (p1, p2, mapOp(c))
            where
              mapOp c | c == '+' = OpAdd
                      | c == '-' = OpMin
                      | c == '*' = OpMul
                      | c == '/' = OpDiv
                      | c == '%' = OpMod

plusOp :: Parser Token
plusOp = do
          p1 <- getPosition
          o <- (try $ string "+=") <|> (string "+")
          p2 <- getPosition
          return (p1, p2, mapOp o)
          where
            mapOp o | o == "+=" = PlusAssign
                    | o == "+" = OpAdd

minOp :: Parser Token
minOp = do
          p1 <- getPosition
          o <- (try $ string "-+") <|> (string "-")
          p2 <- getPosition
          return (p1, p2, mapOp o)
          where
            mapOp o | o == "-=" = MinusAssign
                    | o == "-" = OpMin

relOp :: Parser Token
relOp = do
          p1 <- getPosition
          o <- (try $ string "<=") <|> (try $ string ">=") <|> (string "<") <|> (string ">") 
          p2 <- getPosition
          return (p1, p2, mapOp o)
          where
            mapOp o | o == "<=" = OpLTE
                    | o == ">=" = OpGTE
                    | o == "<" = OpLT
                    | o == ">" = OpGT
            
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
                p1 <- getPosition
                h <- letter <|> char '_'
                r <- many (letter <|> digit <|> char '_')
                p2 <- getPosition
                let str = [h] ++ r
                return $ (p1, p2, makeIdentifier(str))

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
                p1 <- getPosition
                char '\''
                c <- chrQuoted
                char '\''
                p2 <- getPosition
                return $ (p1, p2, CharLit c)

strLiteral :: Parser Token
strLiteral = do
              p1 <- getPosition
              char '"'
              s <- many (strQuoted)
              char '"'
              p2 <- getPosition
              return $ (p1, p2, StrLit s)

chrQuoted = try (char '\\' >> ((oneOf "\\\'\"" >>= return) <|> (char 'n' >> return '\n') <|> (char 't' >> return '\t'))) <|> noneOf "\"\'\t\n" <?> "quoted character"

strQuoted = try (char '\\' >> ((oneOf "\\\'\"" >>= return) <|> (char 'n' >> return '\n') <|> (char 't' >> return '\t'))) <|> noneOf "\"\'\t\n" <?> "quoted character"

numLiteral :: Parser Token
numLiteral = do
                do
                  p1 <- getPosition
                  try $ string "0x"
                  d <- many1 (oneOf "abcdefABCDEF0123456789")
                  p2 <- getPosition
                  return $ (p1, p2, HexLit d)
                <|> do
                      p1 <- getPosition
                      d <- many1 digit
                      p2 <- getPosition
                      return $ (p1, p2, DecLit d)

--------------------------------------------
-- whitespace
--------------------------------------------
--
eol :: Parser ()
eol = (try eof) <|> (char '\n' >> return ())

comment = do
          header <- try $ string "//"
          comment'

comment' :: Parser ()
comment' = eol <|> (eatN 1 >> comment')

sp :: Parser ()
sp = ((try $ space) >> eol) <|> (space >> return ()) <|> (char '\n' >> return ())

whitespace :: Parser ()
whitespace = comment <|> (space >> return ())

ws :: Parser ()
ws = skipMany whitespace
