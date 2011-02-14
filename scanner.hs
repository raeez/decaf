module Scanner
where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos

type Token = (SourcePos, DecafToken)

instance Show DecafToken where
  show (StrLit s) = "STRINGLITERAL \"" ++ s ++ "\""
  show (CharLit s) = "CHARLITERAL \'" ++ show s ++ "\'"
  show (DecLit s) = "INTLITERAL " ++ s
  show (HexLit s) = "INTLITERAL 0x" ++ s
  show (BoolLit s) = "BOOLEANLITERAL " ++ show s
  show (Identf s) = "IDENTIFIER " ++ s
  show (Reserv s) = "RIDENTIFIER " ++ s
  show (LParen) = "("
  show (RParen) = ")"
  show (RBrace) = "{"
  show (LBrace) = "}"
  show (semi) = ";"

showToken :: ((String, Int, Int), DecafToken) -> String
showToken ((name, line, column), t) = (show line) ++ " " ++ (show t)
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
                | Semi
                deriving (Eq)

singleToken :: Parser Token
singleToken = literal
          <|> identifier

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
