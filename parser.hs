module Parser
where
import Text.ParserCombinators.Parsec
import Scanner

data DecafProgram = DecafProgram [DecafField] [DecafMethod]
                  deriving (Show, Eq)

data DecafField = DecafVarField DecafVar
               | DecafArrField DecafArr
               deriving (Show, Eq)

data DecafMethod = DecafMethod DecafType DecafIdentifier [DecafVar] DecafBlock
                 deriving (Show, Eq)

data DecafVar = DecafVar DecafType DecafIdentifier
                  deriving (Show, Eq)

data DecafBlock = DecafBlock [DecafVar] [DecafStm]
                deriving (Show, Eq)

data DecafArr = DecafArr DecafType DecafIdentifier DInt
                  deriving (Show, Eq)

data DecafType = DInteger
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

data DecafMethodCall = DecafMethodCall DecafIdentifier [DecafExpr]
                     | DecafMethodCallout DStr [DecafCalloutArg]
                     deriving (Show, Eq)

data DecafLoc = DecafVarLoc DecafIdentifier
              | DecafArrLoc DecafIdentifier DecafExpr
              deriving (Show, Eq)

data DecafExpr = DecafExpr' DecafExpr' (Maybe [(DecafBinOp, DecafExpr')])
               deriving (Show, Eq)

data DecafExpr' = DecafTermExpr DecafTermExpr
                | DecafParenExpr DecafExpr
                | DecafNotExpr DecafExpr'
                | DecafMinExpr DecafExpr'
                deriving (Show, Eq)

data DecafTermExpr = DecafLocExpr DecafLoc
                   | DecafMethodExpr DecafMethodCall
                   | DecafLitExpr DecafLiteral
                   deriving (Show, Eq)

data DecafCalloutArg = DecafCalloutArgExpr DecafExpr
                     | DecafCalloutArgStr DStr
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
                | DecafLTEOp
                | DecafGTEOp
                deriving (Show, Eq)

data DecafEqOp = DecafEqOp
               | DecafNEqOp
               deriving (Show, Eq)

data DecafCondOp = DecafAndOp
                 | DecafOrOp
                 deriving (Show, Eq)

data DecafIdentifier = DecafIdentifier String
                     | DecafKeyword String
                     deriving (Show, Eq)

data DecafLiteral = DecafIntLit DInt
               | DecafBoolLit Bool 
               | DecafStrLit  DStr
               | DecafCharLit  DChar
               deriving (Show, Eq)

data DStr = DStr String
          deriving (Show, Eq)

data DInt = DDec String
          | DHex  String
          deriving (Show, Eq)

data DBool = DTrue 
           | DFalse
           deriving (Show, Eq)

data DChar = DChar Char
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
                              Identf n -> case n == name of
                                             False -> Nothing
                                             True  -> Just name
                              other                  -> Nothing)

varident = dtoken (\tok -> case tok of
                            Identf name -> Just name
                            other -> Nothing)

reserv name = dtoken (\tok -> case tok of
                                  Reserv s | s == name -> Just name
                                  other                -> Nothing)

strlit = dtoken (\tok -> case tok of
                          StrLit s -> Just s
                          other    -> Nothing)

int = (hexlit >>= return . DHex) <|> (declit >>= return . DDec)

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
                            BoolLit True -> Just True
                            BoolLit False -> Just False
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

parser = ps program

ps p i = case parse p "decaf-parser" (eatFirst i) of
        Left err -> show err
        Right val -> show val


program = do
            reserv "class" >> ident "Program"
            lbrace
            f <- many $ (try $ fielddecl)
            m <- many $ ( methoddecl)
            rbrace
            return $ DecafProgram (foldr (++) [] f) m

fielddecl = do
              t <- vartype
              fd <- (fdecl t) `sepBy` comma
              semi
              return fd

fdecl t = (try $ adecl t) <|> (vdecl t)

vdecl t = do
            i <- identvar
            return $ DecafVarField $ DecafVar t i

adecl t = do
            i <- identvar
            lbrack
            s <- int
            rbrack
            return $ DecafArrField $ DecafArr t i s

methoddecl = do
              t <- rettype
              i <- identvar
              lparen
              p <- pdecl `sepBy` comma
              rparen
              b <- block
              return $ DecafMethod t i p b

pdecl = do
          t <- vartype
          i <- identvar
          return $ DecafVar t i

block = do
          lbrace
          v <- many vardecl
          s <- many statement
          rbrace
          return $ DecafBlock (foldr (++) [] v) s

vardecl = do
            t <- vartype
            i <- identvar `sepBy` comma
            semi
            return $ (map (DecafVar t) i)

identvar = (varident >>= return . DecafIdentifier)

voidtype = (reserv "void" >> return DVoid)

integertype = (reserv "int" >> return DInteger)
booleantype = (reserv "boolean" >> return DBoolean)
vartype = integertype <|> booleantype
rettype = vartype <|> voidtype

statement =  try (do
               l <- location
               o <- assignop
               e <- expr
               semi
               return $ DecafAssignStm l o e)
         <|> try (do
               m <- methodcall
               semi
               return $ DecafMethodStm m)
         <|> (do
               reserv "if"
               lparen
               e <- expr
               rparen
               b1 <- block
               b <- mayb (reserv "else" >> block)
               return $ DecafIfStm e b1 b)
         <|> (do
                reserv "for"
                i <- identvar
                aseq
                e1 <- expr
                comma
                e2 <- expr
                b <- block
                return $ DecafForStm i e1 e2 b)
         <|> (do
                reserv "return"
                e <- mayb expr
                semi
                return $ DecafRetStm e)
         <|> (reserv "break" >> semi >> return DecafBreakStm)
         <|> (reserv "continue" >> semi >> return DecafContStm)
         <|> (block >>= return . DecafBlockStm)

methodcall = (do
                reserv "callout"
                lparen
                s <- slit
                comma <|> (notFollowedBy eof)
                p <- calloutarg `sepBy` comma
                rparen
                return $ DecafMethodCallout s p)
          <|> (do
                i <- identvar
                lparen
                p <- expr `sepBy` comma
                rparen
                return $ DecafMethodCall i p)

-- left associative, right recursive

mayb x = do
          (x >>= return . Just) <|> (return Nothing)

maybl x = do
          m <- many x
          return $ case (length m > 0) of
                    True -> Just m
                    False -> Nothing
expr = do
      e <- try expr'
      t <- maybl exprTail
      return $ DecafExpr' e t

exprTail = do
            b <- binop
            e <- expr'
            return (b, e)

expr' = (terminal >>= return . DecafTermExpr)
  <|> (do 
         lparen
         e <- expr
         rparen
         return $ DecafParenExpr e)

 <|> (do
        opnot
        e <- expr'
        return $ DecafNotExpr e)
 <|> (do
        opmin
        e <- expr'
        return $ DecafMinExpr e)

terminal = try (methodcall >>= return . DecafMethodExpr)
    <|> (location >>= return . DecafLocExpr)
    <|> (lit >>= return . DecafLitExpr)

binop = (arithop >>= return . DecafBinArithOp)
     <|> (relop >>= return . DecafBinRelOp)
     <|> (eqop >>= return . DecafBinEqOp)
     <|> (condop >>= return . DecafBinCondOp)

arithop = addop
       <|> subop
       <|> mulop
       <|> divop
       <|> modop

addop = (opadd >> return DecafPlusOp)
subop = (opmin >> return DecafMinOp)
mulop = (opmul >> return DecafMulOp)
divop = (opdiv >> return DecafDivOp)
modop = (opmod >> return DecafModOp)

relop = ltop
     <|> gtop
     <|> lteop
     <|> gteop

ltop = (oplt >> return DecafLTOp)
gtop = (opgt >> return DecafGTOp)
lteop = (oplte >> return DecafLTEOp)
gteop = (opgte >> return DecafGTEOp)

eqop = oeq
    <|> oneq

oeq = (opeq >> return DecafEqOp)
oneq = (opneq >> return DecafNEqOp)

condop = andop
      <|> orop

andop = (opand >> return DecafAndOp)
orop = (opor >> return DecafOrOp)

assignop = aseq
        <|> mineq
        <|> pluseq

aseq = (assign >> return DecafEq)
mineq = (minusassign >> return DecafMinusEq)
pluseq = (plusassign >> return DecafPlusEq)

lit = ilit <|> clit  <|> blit
slit = (strlit >>= return . DStr)
ilit = (int >>= return . DecafIntLit)
blit = (boollit >>= return . DecafBoolLit)
clit = (chrlit >>= return . DecafCharLit . DChar)

location = (try arrlocation) <|> varlocation

varlocation = do
                i <- identvar
                return $ DecafVarLoc i

arrlocation = do
                i <- identvar
                lbrack
                e <- expr
                rbrack
                return $ DecafArrLoc i e

calloutarg = (expr >>= return . DecafCalloutArgExpr) <|> (slit >>= return . DecafCalloutArgStr)
