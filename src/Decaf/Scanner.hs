module Decaf.Scanner
where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import Text.ParserCombinators.Parsec.Error
import Text.Regex.Posix
import Data.List
import Decaf.Tokens

--------------------------------------------
-- interface functions
--------------------------------------------
--

-- |The 'scanner' function takes and generates a corresponding list of Token
scanner :: String -> [Token]
scanner = eatFirst

-- |The 'scprint' function scans a given string and returns provides formatted output
scprint :: String -> String
scprint = formatScannerOutput . scanner

-- |The 'formatScannerOutput' function formats a list of scanned tokens into human readable format
formatScannerOutput :: [Token] -> String
formatScannerOutput = unlines . (map showToken)

-- |The 'numLexErrorsIn' function calculates the number of errors in a list of tokens
numLexErrorsIn :: [Token] -> Int
numLexErrorsIn tokens = numErrors tokens' 0
                        where
                          tokens' = map getToken tokens

numErrors :: [DecafToken] -> Int -> Int
numErrors (t:ts) eCount = numErrors ts eCount'
                          where
                            eCount' = case t of
                                        (Fail s) -> eCount + 1
                                        otherwise -> eCount
numErrors [] eCount = eCount

--------------------------------------------
-- general
--------------------------------------------
--
eatN :: Int -> Parser ()
eatN 0 = (return ())
eatN n = anyChar >> (eatN (n-1))

--------------------------------------------
-- whitespace
--------------------------------------------
--
eol :: Parser ()
eol = (try eof) <|> (char '\n' >> return ())
   <?> "end of line"

comment' :: Parser ()
comment' = eol <|> (eatN 1 >> comment')
        <?> "comment"

whitespace :: Parser ()
whitespace = try (string "//" >> comment') <|> (space >> return ())

ws :: Parser ()
ws = skipMany whitespace
  <?> "whitespace"

--------------------------------------------
-- literals
--------------------------------------------
--
literal :: Parser Token
literal = chrLiteral
       <|> strLiteral
       <|> numLiteral
       <?> "literal"

chrLiteral :: Parser Token
chrLiteral = do
                p1 <- getPosition
                char '\''
                c <- quoted
                char '\''
                p2 <- getPosition
                return $ ((p1, p2), CharLit c)
          <?> "character literal"

strLiteral :: Parser Token
strLiteral = do
              p1 <- getPosition
              char '"'
              s <- many quoted
              char '"'
              p2 <- getPosition
              return $ ((p1, p2), StrLit s)

quoted = try (char '\\' >> ((oneOf "\\\'\"" >>= return)
                          <|> (char 'n' >> return '\n')
                          <|> (char 't' >> return '\t')))
         <|> noneOf "\"\'\t\n"
         <?> "quoted character"

numLiteral :: Parser Token
numLiteral = (do
                p1 <- getPosition
                try $ string "0x"
                d <- many1 hexDigit
                p2 <- getPosition
                return $ ((p1, p2), HexLit d))
          <|> (do
                 p1 <- getPosition
                 d <- many1 digit
                 p2 <- getPosition
                 return $ ((p1, p2), DecLit d))
          <?> "integer literal"

--------------------------------------------
-- identifiers
--------------------------------------------
--

makeIdentifier :: String -> DecafToken
makeIdentifier (t) = case (t `elem` keywords) of
                                  False -> Identf t
                                  otherwise -> case (t `elem` bools) of
                                                False -> Reserv t
                                                otherwise -> case (t == "true") of
                                                              True -> BoolLit True
                                                              False -> BoolLit False
                      where
                        bools = [
                          "true",
                          "false"
                          ]
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
                return $ ((p1, p2), makeIdentifier(str))
          <?> "identifier"

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
        <?> "operator"

notOp :: Parser Token
notOp = do
          p1 <- getPosition
          o <- (try $ string "!=") <|> string "!"
          p2 <- getPosition
          return ((p1, p2), mapOp o)
          where
            mapOp o | o == "!=" = OpNEq
                    | o == "!" = OpNot

eqOp :: Parser Token
eqOp = do
        p1 <- getPosition
        o <- (try $ string "==") <|> (string "=")
        p2 <- getPosition
        return ((p1, p2), mapOp o)
        where
          mapOp o | o == "==" = OpEq
                  | o == "=" = Assign

condOp :: Parser Token
condOp = do
          p1 <- getPosition
          o <- (string "&&") <|> (string "||")
          p2 <- getPosition
          return ((p1, p2), mapOp o)
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
            return ((p1, p2), mapOp(c))
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
          return ((p1, p2), mapOp o)
          where
            mapOp o | o == "+=" = PlusAssign
                    | o == "+" = OpAdd

minOp :: Parser Token
minOp = do
          p1 <- getPosition
          o <- (try $ string "-=") <|> (string "-")
          p2 <- getPosition
          return ((p1, p2), mapOp o)
          where
            mapOp o | o == "-=" = MinusAssign
                    | o == "-" = OpMin

relOp :: Parser Token
relOp = do
          p1 <- getPosition
          o <- (try $ string "<=") <|> (try $ string ">=") <|> (string "<") <|> (string ">") 
          p2 <- getPosition
          return ((p1, p2), mapOp o)
          where
            mapOp o | o == "<=" = OpLTE
                    | o == ">=" = OpGTE
                    | o == "<" = OpLT
                    | o == ">" = OpGT
            
--------------------------------------------
-- terminals
--------------------------------------------
--
terminal :: Parser Token
terminal = do
          p1 <- getPosition
          b <- char ';' <|> char '[' <|> char ']' <|> char '(' <|> char ')' <|> char '{' <|> char '}' <|> char ','
          p2 <- getPosition
          return ((p1, p2), mapTerm b)
          where
            mapTerm b
                | b == ';' = Semi
                | b == '[' = LBrack
                | b == ']' = RBrack
                | b == '(' = LParen
                | b == ')' = RParen
                | b == '{' = LBrace
                | b == '}' = RBrace
                | b == ',' = Comma

--------------------------------------------
-- Navigation
--------------------------------------------
--
--determine whether to start before or after the detected error
beforeOrAfter :: ParseError -> Int
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

singleToken :: Parser Token
singleToken = do
                ws
                t <- (operator <|> literal <|> identifier <|> terminal <|> end)
                ws
                return t


end :: Parser Token
end = do
        p1 <- getPosition
        eof
        p2 <- getPosition
        return ((p1, p2), EOF)

--------------------------------------------
-- scanner
--------------------------------------------
--
eatNext :: Parser Token -> String -> [Token]
eatNext parser input = case parse parser "decaf-scanner-eatNext" input of
                        Left err -> let errPosition   = (errorPos err, incSourceColumn (errorPos err) (1 + beforeOrAfter err))
                                        errDecafToken = Fail $ show err
                                        errToken      = (errPosition, errDecafToken)
                                    in [errToken] ++ eatNext(failParser err) input

                        Right val -> case (getToken val == EOF) of
                                      False -> [val] ++ (eatNext (successParser val) input)
                                      otherwise -> []
                        where
                          failParser    e = (eatPos input $ incSourceColumn (errorPos e) $ beforeOrAfter e) >> (singleToken)
                          successParser v = (eatPos input $ getEnd v) >> (singleToken)

eatFirst :: String -> [Token]
eatFirst inp = case parse (singleToken) "decaf-scanner-eatFirst" input of
                  Left err -> let errPosition   = (errorPos err, incSourceColumn (errorPos err) (1 + (beforeOrAfter err)))
                                  errDecafToken = Fail $ show err
                                  errToken      = (errPosition, errDecafToken)
                              in [errToken] ++ eatNext(failParser err) input

                  Right val -> case (getToken val == EOF) of
                                False -> [val] ++ (eatNext (successParser val) input)
                                otherwise -> []
                  where
                    input = clean
                    clean = map fix inp
                    fix c | c == '\t' = ' '
                          | otherwise = c
                    failParser    e = (eatPos input $ incSourceColumn (errorPos e) $ beforeOrAfter e) >> (singleToken)
                    successParser e = (eatPos input $ getEnd e) >> (singleToken)
