module Parser
where
import Text.ParserCombinators.Parsec
import Scanner

data DecafProgram = DecafProgram {
                                  fields :: [DecafField],
                                  methods :: [DecafMethod]
                                  }

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

ident name = dtoken (\tok -> case tok of
                              Identf name -> Just name
                              other       -> Nothing)

reserv name = dtoken (\tok -> case tok of
                                  Reserv s | s == name -> Just name
                                  other                -> Nothing)

strlit = dtoken (\tok -> case tok of
                          StrLit s -> Just s
                          other    -> Nothing)

hexlit = dtoken (\tok -> case tok of
                          HexLit s -> Just s
                          other    -> Nothing)

declit = dtoken (\tok -> case tok of
                            DecLit s -> Just s
                            other    -> Nothing)

chrlit = dtoken (\tok -> case tok of
                            CharLit c -> Just c
                            other -> Nothing)

boollit = dtoken (\tok -> case tok of
                            BoolLit b -> Just b
                            other -> Nothing)

lparen = dtoken (\tok -> case tok of
                          LParen -> Just ()
                          other -> Nothing)
rparen = dtoken (\tok -> case tok of
                          RParen -> Just ()
                          other -> Nothing)

lbrace = dtoken (\tok -> case tok of
                          LBrace -> Just ()
                          other -> Nothing)

rbrace = dtoken (\tok -> case tok of
                          RBrace -> Just ()
                          other -> Nothing)

lbrack = dtoken (\tok -> case tok of
                            LBrack -> Just ()
                            other -> Nothing)

rbrack = dtoken (\tok -> case tok of
                            RBrack -> Just ()
                            other -> Nothing)

comma = dtoken (\tok -> case tok of
                            Comma -> Just ()
                            other -> Nothing)

semi = dtoken (\tok -> case tok of
                        Semi -> Just ()
                        other -> Nothing)

opand = dtoken (\tok -> case tok of
                          OpAnd -> Just ()
                          other -> Nothing)

opor = dtoken (\tok -> case tok of
                          OpOr -> Just ()
                          other -> Nothing)

opeq = dtoken (\tok -> case tok of
                          OpEq -> Just ()
                          other -> Nothing)

opneq = dtoken (\tok -> case tok of
                          OpNEq -> Just ()
                          other -> Nothing)
                            
oplt = dtoken (\tok -> case tok of
                          OpLT -> Just ()
                          other -> Nothing)

opgt = dtoken (\tok -> case tok of
                          OpGT -> Just ()
                          other -> Nothing)

oplte = dtoken (\tok -> case tok of
                          OpLTE-> Just ()
                          other -> Nothing)

opgte = dtoken (\tok -> case tok of
                          OpGTE-> Just ()
                          other -> Nothing)

opadd = dtoken (\tok -> case tok of
                          OpAdd-> Just ()
                          other -> Nothing)

opmin = dtoken (\tok -> case tok of
                          OpMin-> Just ()
                          other -> Nothing)

opmul = dtoken (\tok -> case tok of
                          OpMul -> Just ()
                          other -> Nothing)

opdiv = dtoken (\tok -> case tok of
                          OpDiv -> Just ()
                          other -> Nothing)

opmod = dtoken (\tok -> case tok of
                          OpMod -> Just ()
                          other -> Nothing)

opnot = dtoken (\tok -> case tok of
                          OpNot -> Just ()
                          other -> Nothing)

assign = dtoken (\tok -> case tok of
                          Assign -> Just ()
                          other -> Nothing)

plusassign = dtoken (\tok -> case tok of
                          PlusAssign -> Just ()
                          other -> Nothing)

minusassign = dtoken (\tok -> case tok of
                          MinusAssign -> Just ()
                          other -> Nothing)
parseStream tokenStream = case parse program "decaf-parser" tokenStream of
                            Left err -> Nothing
                            Right val -> Just val
program = do
            reserv "class" >> ident "Program"
            reserv "{"
            reserv "}"
            return $ DecafProgram [] []
