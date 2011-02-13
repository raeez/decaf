module Scanner
where
import Text.ParserCombinators.Parsec

type Token = (SourcePos, DecafToken)

data DecafToken = Identf String
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
                deriving (Show, Eq)

--------------------------------------------
-- literals
--------------------------------------------
--
stream :: Parser Token
stream = chrLiteral
       <|> strLiteral
       <|> numLiteral
       <|> boolLiteral

chrLiteral :: Parser Token
chrLiteral = do
                char '\''
                p <- getPosition
                c <- anyChar
                char '\'' <?> "character literal"
                return $ (p, CharLit c)

strLiteral :: Parser Token
strLiteral = do
              char '"'
              p <- getPosition
              s <- many (quoted)
              char '"' <?> "string literal"
              return $ (p, StrLit s)
              where
                quoted = try (char '\\' >> oneOf "\'\"\t\n" >>= return)
                  <|> noneOf "\""
                  <?> "quoted character"

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
                <?> "integer literal"

boolLiteral :: Parser Token
boolLiteral = do
                do
                  p <- getPosition
                  string "true"
                  return $ (p, BoolLit True)
                <|> do
                      p <-getPosition
                      string "false"
                      return $ (p, BoolLit False)
                <?> "boolean literal"

--------------------------------------------
-- whitespace
--------------------------------------------
--
comment :: Parser String
comment = do
            string "//"
            skipMany (noneOf "\r\n")
            eol
          <?> "comment"

eol = try (string "\n\r") -- k = 2 lookahead
    <|> try (string "\r\n") -- k = 2 lookahead
    <|> string "\n"
    <|> string "\r"
    <?> "EOL"


ws :: Parser ()
ws = do
       skipMany space <|> skipMany comment
       <?> "ws"

separator :: Parser ()
separator = do
              skipMany1 space <|> skipMany1 comment <|> eof
              ws
            <?> "ws"
