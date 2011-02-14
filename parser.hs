module Parser
where
import Text.ParserCombinators.Parsec
import Scanner

data DecafProgram = DecafProgram [DecafField] [DecafMethod] deriving (Show, Eq)

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

type DecafParser a = GenParser Token () a

dtoken :: (DecafToken -> Maybe a) -> DecafParser a
dtoken test = token showTok posTok testTok
              where
                showTok (p1, p2, t) = show t
                posTok (p1, p2, t) = p1
                testTok (p1, p2, t) = test t

identifier :: DecafParser String
identifier = dtoken (\tok -> case tok of
                              Identf name -> Just name
                              other       -> Nothing)

reserved :: DecafParser ()
reserved name = dtoken (\tok -> case tok of
                                  Reserv s | s == name -> Just ()
                                  other                  -> Nothing)

parser
