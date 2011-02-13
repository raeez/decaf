module Decaf
where
import Text.ParserCombinators.Parsec
import Numeric
import Monad

data DecafProgram = DecafProgram [DecafField] [DecafMethod]
               deriving (Show, Eq)

data DecafField = DecafVarField DecafVarDecl 
               | DecafArrayField DecafArrDecl
               deriving (Show, Eq)

data DecafMethod = DecafMethod DecafType DID (Maybe [DecafVar]) DecafBlock
                  deriving (Show, Eq)

data DecafBlock = DecafBlock [DecafVarDecl] [DecafStm]
                  deriving (Show, Eq)

data DecafVarDecl = DecafVarDecl DecafType DID
                DecafVarListDecl DecafType [DID]
                deriving (Show, Eq)

data DecafArrDecl = DecafArrDecl DecafType DID DNum
                deriving (Show, Eq)

data DecafType = DInteger
               | DBoolean
               | DVoid
               deriving (Show, Eq)

data DecafStm = DecafAssignStm DecafLoc DecafAssignOp DecafExpr
              | DecafMethodStm DecafMethodCall
              | DecafIfStm DecafExpr DecafBlock (Maybe DecafBlock)
              | DecafForStm DID DecafExpr DecafExpr DecafBlock
              | DecafRetStm (Maybe DecafExpr)
              | DecafBreakStm
              | DecafContStm
              | DecafBlockStm DecafBlock
              deriving (Show, Eq)

data DecafAssignOp = DecafEq
                   | DecafPlusEq
                   | DecafMinusEq
                   deriving (Show, Eq)

data DecafMethodCall = DecafMethodCall DID (Maybe [DecafExpr])
                     | DecafMethodCallout DStr (Maybe [DecafCalloutArg])
                     deriving (Show, Eq)

data DecafLoc = DecafVar DID
              | DecafArr DID DecafExpr
              deriving (Show, Eq)

data DecafExpr = DecafFieldExpr DecafField
               | DecafMethodExpr DecafMethodCall
               | DecafLitExpr DecafLiteral
               | DecafOpExpr DecafExpr DecafOp DecafExpr
               | DecafMinExpr DecafExpr
               | DecafBangExpr DecafExpr
               | DecafParenExpr DecafExpr
               deriving (Show, Eq)

data DecafCalloutArg = DecafCalloutArgExpr DecafExpr
                     | DecafCalloutArgStr DSt

data DecafBinOp = DecafBinArithOp DecafArithOp
                | DecafBinRelOp DecafRelOp
                | DecafBinEqOp DecafEqOp
                | DecafBinCondOp DecafCondOp
                deriving (Show, Eq)

data DecafArithOp = DecafPlusOp
                  | DecafMinOp
                  | DecafMulOp
                  | DecafDivOp
                  | DecafModOp
                  deriving (Show, Eq)

data DecafRelOp = DecafLTOp
                | DecafGTOp
                | DecafLTEqOp
                | DecafGTEqOp
                deriving (Show, Eq)

data DecafEqOp = DecafEqOp
               | DecafNotEqOp
               deriving (Show, Eq)

data DecafCondOp = DecafAndOp
                 | DecafOrOp
                 deriving (Show, Eq)

DecafIdentifier = DID String

data DecafLiteral = DNum DNumLit
               | DBool Bool
               | DStr String
               | DChar Char
               deriving (Show, Eq)

data DNumLit = DInt Int
             | DHex Int

data ParserErrorMessage = ParserErrorMessage String
                | ParserErrorMessageList [ParserErrorMessage]
                deriving (Show, Eq)

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


--------------------------------------------
-- scanner/parser
--

--------------------------------------------
-- ws / comments
--
-- grammar
parseDecafProgram :: Parser DecafProgram
parseDecafProgram = do
                  ws
                  string "class"
                  ws
                  string "Program"
                  ws
                  string "{"
                  ws
                  fields <- many parseDecafFieldDecl
                  ws
                  methods <- many parseDecafMethodDecl
                  ws
                  string "}"
                  ws
                  return $ DecafProg fields methods

parseDecafFieldDecl :: Parser DecafField
parseDecafFieldDecl = do
                    ws
                    t <- typeDecl
                    ws
                    i <- many (parseDecafFields)
                    ws
                    return $ DecafField t i

parseMethodDecl :: Parser MethodDecl
parseMethodDecl = do
                    ws
                    t <- typeDecl
                    ws
                    i <- idDecl
                    ws
                    char '('
                    i <- intLiteral
                    char ')'

parseBlock = do
              char '{'
              vs <- many(parseVarDecl)
              ss <- many(parseStmDecl)
              char '}'
              return $ DecafBlock vs ss

parseVarDecl = 

--------------------------------------------
-- identifiers
--
identifier :: Parser DID
identifier = do
              first <- char letter
              rest <- char letter <|> char digit
              let ident <- [first] ++ rest in
              return $ DID ident

alphaNumeric :: Parser Char
alphaNumeric = do
                h <- many (letter <|> char '_')
                r <- many (letter <|> digit <|> char '_')
                return $ [h] ++ r

--------------------------------------------
-- literals
--
literal :: Parser DecafLiteral
literal = charLiteral
       <|> strLiteral
       <|> numLiteral
       <|> boolLiteral

charLiteral :: Parser DecafLiteral
charLiteral = do
                char '\''
                c <- anyChar
                char '\'' <?> "character literal"
                return $ DChar c

strLiteral :: Parser DecafLiteral
strLiteral = do
              char '"'
              s <- many (quoted)
              char '"' <?> "string literal"
              return $ DStr s
              where
                quoted = try (char '\\' >> anyChar >>= return) -- TODO only \', \", \t, \n, \t
                  <|> noneOf "\""
                  <?> "quoted character"

numLiteral :: Parser DecafLiteral
numLiteral = do
                (try $ string "0x" >> many1 hexDigit >>= return . DNum . DHex . fst . head . readHex)
                <|> (many1 digit >>= return . DNum . DInt . read)
                <?> "integer literal"

boolLiteral :: Parser DecafLiteral
boolLiteral = do
                (string "true" >> return DTrue) <|> (string "false" >> return DFalse)
                <?> "boolean literal"

--------------------------------------------
-- whitespace
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

dead :: Parser Char
dead = char '\n'
    <|> char '\r'
    <|> char '\t'
    <|> space
    <?> "ws"

ws :: Parser ()
ws = do
              skipMany dead <|> skipMany comment
            <?> "ws"

separator :: Parser ()
separator = do
              skipMany1 dead <|> skipMany1 comment
              ws
            <?> "ws"
