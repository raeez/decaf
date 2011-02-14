module Decaf
where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Numeric
import Monad

data DecafProgram = DecafProgram [DecafField] [DecafMethod]
                  deriving (Show, Eq)

data DecafField = DecafVarField DecafVarDecl 
               | DecafArrayField DecafArrDecl
               deriving (Show, Eq)

data DecafMethod = DecafMethod DecafType DecafIdentifier (Maybe [DecafVarDecl]) DecafBlock
                 deriving (Show, Eq)

data DecafBlock = DecafBlock [DecafVarDecl] [DecafStm]
                deriving (Show, Eq)

data DecafVarDecl = DecafVarDecl DecafType DecafIdentifier 
                  | DecafVarListDecl DecafType [DecafIdentifier]
                  deriving (Show, Eq)

data DecafArrDecl = DecafArrDecl DecafType DecafIdentifier DNumLit 
                  deriving (Show, Eq)

data DecafType = DInt
               | DBoolean
               | DVoid
               deriving (Show, Eq)

data DecafStm = DecafAssignStm DecafLoc DecafAssignOp DecafExpr
              | DecafMethodStm DecafMethodCall
              | DecafIfStm DecafExpr DecafBlock (Maybe DecafBlock)
              | DecafForStm DecafIdentifier DecafExpr DecafExpr DecafBlock
              | DecafRetStm (Maybe DecafExpr)
              | DecafBreakStm
              | DecafContStm
              | DecafBlockStm DecafBlock
              deriving (Show, Eq)

data DecafAssignOp = DecafEq
                   | DecafPlusEq
                   | DecafMinusEq
                   deriving (Show, Eq)

data DecafMethodCall = DecafMethodCall DecafIdentifier (Maybe [DecafExpr])
                     | DecafMethodCallout DStrLit (Maybe [DecafCalloutArg])
                     deriving (Show, Eq)

data DecafLoc = DecafVar DecafIdentifier
              | DecafArr DecafIdentifier DecafExpr
              deriving (Show, Eq)

data DecafExpr = DecafFieldExpr DecafField
               | DecafMethodExpr DecafMethodCall
               | DecafLitExpr DecafLiteral
               | DecafOpExpr DecafExpr DecafBinOp DecafExpr
               | DecafMinExpr DecafExpr
               | DecafBangExpr DecafExpr
               | DecafParenExpr DecafExpr
               deriving (Show, Eq)

data DecafCalloutArg = DecafCalloutArgExpr DecafExpr
                     | DecafCalloutArgStr DStrLit
                     deriving (Show, Eq)

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

data DecafIdentifier = DecafID String
                     | DecafKeyword String
                     deriving (Show, Eq)

data DecafLiteral = DNumLit DNumLit
               | DBoolLit DBoolLit
               | DStrLit DStrLit 
               | DCharLit DCharLit
               deriving (Show, Eq)

data DStrLit = DStr String
             deriving (Show, Eq)

data DNumLit = DDec Int
             | DHex Int
             deriving (Show, Eq)

data DBoolLit = DTrue 
              | DFalse
              deriving (Show, Eq)

data DCharLit = DChar Char
              deriving (Show, Eq)

data ParserErrorMessage = ParserErrorMessage String
                | ParserErrorMessageList [ParserErrorMessage]
                deriving (Show, Eq)


--------------------------------------------
-- scanner/parser
--

--------------------------------------------
-- ws / comments
--
-- grammar
--parseDecafProgram :: Parser DecafProgram
--parseDecafProgram = do
                  --ws
                  --string "class"
                  --ws
                  --string "Program"
                  --ws
                  --string "{"
                  --ws
                  --fields <- many parseDecafFieldDecl
                  --ws
                  --methods <- many parseDecafMethodDecl
                  --ws
                  --string "}"
                  --ws
                  --return $ DecafProg fields methods
--
--parseDecafFieldDecl :: Parser DecafField
--parseDecafFieldDecl = do
                    --ws
                    --t <- typeDecl
                    --ws
                    --i <- many (parseDecafFields)
                    --ws
                    --return $ DecafField t i
--
--parseMethodDecl :: Parser MethodDecl
--parseMethodDecl = do
                    --ws
                    --t <- typeDecl
                    --ws
                    --i <- idDecl
                    --ws
                    --char '('
                    --i <- intLiteral
                    --char ')'
--
--parseBlock = do
              --char '{'
              --vs <- many(parseVarDecl)
              --ss <- many(parseStmDecl)
              --char '}'
              --return $ DecafBlock vs ss

--parseVarDecl = 

--------------------------------------------
-- expressions
--
expr :: Parser Integer
expr = buildExpressionParser table factor
    <?> "expression"

table = [[op "*" (*) AssocLeft, op "/" div AssocLeft],
         [op "+" (+) AssocLeft, op "-" (-) AssocLeft]]
        where
          op s f assoc = Infix (string s >> return f) assoc

factor = do
          char '('
          x <- expr
          char ')'
          return x
      <|> parseNumLiteral
      <?> "simple expression"
--------------------------------------------
-- types
--
typeDecl :: Parser DecafType
typeDecl =  (string "boolean" >> return DBoolean)
        <|> (string "integer" >> return DInt)
        <|> (string "void" >> return DVoid)
