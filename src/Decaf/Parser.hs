module Decaf.Parser
where
import Text.ParserCombinators.Parsec
import Decaf.Tokens
import Decaf.AST
import Decaf.Scanner

-- custom token parser
type DecafParser a = GenParser Token () a

decafToken :: (DecafToken -> Maybe a) -> DecafParser a
decafToken test = token showToken posToken testToken
                  where
                    showToken (p1, p2, t) = show t
                    posToken (p1, p2, t) = p1
                    testToken (p1, p2, t) = test t

ident name = decafToken (\tok -> case tok of
                              Identf n -> case n == name of
                                             False -> Nothing
                                             True  -> Just name
                              other                  -> Nothing)

varident = decafToken (\tok -> case tok of
                            Identf name -> Just name
                            other -> Nothing)

reserv name = decafToken (\tok -> case tok of
                                  Reserv s | s == name -> Just name
                                  other                -> Nothing)

strlit = decafToken (\tok -> case tok of
                          StrLit s -> Just s
                          other    -> Nothing)

int = (hexlit >>= return . DecafHex) <|> (declit >>= return . DecafDec)

hexlit = decafToken (\tok -> case tok of
                          HexLit s -> Just s
                          other    -> Nothing)

declit = decafToken (\tok -> case tok of
                            DecLit s -> Just s
                            other    -> Nothing)

chrlit = decafToken (\tok -> case tok of
                            CharLit c -> Just c
                            other -> Nothing)

boollit = decafToken (\tok -> case tok of
                            BoolLit True -> Just True
                            BoolLit False -> Just False
                            other -> Nothing)

lparen = decafToken (\tok -> case tok of
                          LParen -> Just ()
                          other -> Nothing)
rparen = decafToken (\tok -> case tok of
                          RParen -> Just ()
                          other -> Nothing)

lbrace = decafToken (\tok -> case tok of
                          LBrace -> Just ()
                          other -> Nothing)

rbrace = decafToken (\tok -> case tok of
                          RBrace -> Just ()
                          other -> Nothing)

lbrack = decafToken (\tok -> case tok of
                            LBrack -> Just ()
                            other -> Nothing)

rbrack = decafToken (\tok -> case tok of
                            RBrack -> Just ()
                            other -> Nothing)

comma = decafToken (\tok -> case tok of
                            Comma -> Just ()
                            other -> Nothing)

semi = decafToken (\tok -> case tok of
                        Semi -> Just ()
                        other -> Nothing)

opand = decafToken (\tok -> case tok of
                          OpAnd -> Just ()
                          other -> Nothing)

opor = decafToken (\tok -> case tok of
                          OpOr -> Just ()
                          other -> Nothing)

opeq = decafToken (\tok -> case tok of
                          OpEq -> Just ()
                          other -> Nothing)

opneq = decafToken (\tok -> case tok of
                          OpNEq -> Just ()
                          other -> Nothing)
                            
oplt = decafToken (\tok -> case tok of
                          OpLT -> Just ()
                          other -> Nothing)

opgt = decafToken (\tok -> case tok of
                          OpGT -> Just ()
                          other -> Nothing)

oplte = decafToken (\tok -> case tok of
                          OpLTE-> Just ()
                          other -> Nothing)

opgte = decafToken (\tok -> case tok of
                          OpGTE-> Just ()
                          other -> Nothing)

opadd = decafToken (\tok -> case tok of
                          OpAdd-> Just ()
                          other -> Nothing)

opmin = decafToken (\tok -> case tok of
                          OpMin-> Just ()
                          other -> Nothing)

opmul = decafToken (\tok -> case tok of
                          OpMul -> Just ()
                          other -> Nothing)

opdiv = decafToken (\tok -> case tok of
                          OpDiv -> Just ()
                          other -> Nothing)

opmod = decafToken (\tok -> case tok of
                          OpMod -> Just ()
                          other -> Nothing)

opnot = decafToken (\tok -> case tok of
                          OpNot -> Just ()
                          other -> Nothing)

assign = decafToken (\tok -> case tok of
                          Assign -> Just ()
                          other -> Nothing)

plusassign = decafToken (\tok -> case tok of
                          PlusAssign -> Just ()
                          other -> Nothing)

minusassign = decafToken (\tok -> case tok of
                          MinusAssign -> Just ()
                          other -> Nothing)

containsErrors (Just a)= False
containsErrors Nothing = True

data Report a = RSuccess a
              | RError String
getSuccess (RSuccess a) = a

getReport (RSuccess a) = show a
getReport (RError s) = s

parser i = getReport $ ps program i

ps p i = case parse p "decaf-parser" (eatFirst i) of
          Left err -> RError $ show err
          Right val -> RSuccess val

qs p i = case parse p "internal-decaf-parser" (eatFirst i) of
        Left err -> Nothing
        Right val -> Just val

program = do
            reserv "class" >> ident "Program"
            lbrace
            f <- many $ (try $ fielddecl)
            m <- many $ ( methoddecl)
            rbrace
            return $ DecafProgram (concat f) m

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
          v <- many (try vardecl)
          s <- many statement
          rbrace
          return $ DecafBlock (concat v) s

vardecl = do
            t <- vartype
            i <- identvar `sepBy1` comma
            semi
            return $ (map (DecafVar t) i)

identvar = (varident >>= return . DecafIdentifier)

voidtype = (reserv "void" >> return DecafVoid)

integertype = (reserv "int" >> return DecafInteger)
booleantype = (reserv "boolean" >> return DecafBooleanean)
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
                comma <|> (return ())
                p <- calloutarg `sepBy` comma
                rparen
                return $ DecafMethodCallout s p)
          <|> (do
                i <- identvar
                lparen
                p <- expr `sepBy` comma
                rparen
                return $ DecafPureMethodCall i p)

-- left associative, right recursive

mayb p = do
          (p >>= return . Just) <|> (return Nothing)

expr = do
        t <- term
        e <- expr'
        return $ DecafExpr t e

expr' = (do
          b <- toplevelOp
          t <- term
          e <- expr'
          return $ Expr' b t e)
     <|> (return $ EmptyExpr')

term = do
         f <- factor
         t <- term'
         return $ Term f t

term' = (do
          b <- botlevelOp
          f <- factor
          t <- term'
          return $ Term' b f t)
     <|> (return $ EmptyTerm')

factor = (try methodcall >>= return . DecafMethodExpr')
      <|> (location >>= return . DecafLocExpr')
      <|> (lit >>= return . DecafLitExpr')
      <|> (opnot >> expr >>= return . DecafNotExpr')
      <|> (opmin >> expr >>= return . DecafMinExpr')
      <|> (do
            lparen
            e <- expr
            rparen
            return $ DecafParenExpr' e)

toplevelOp = ((addop <|> subop) >>= return . DecafBinArithOp) <|> relop <|> eqop <|> condop
botlevelOp = (mulop <|> divop <|> modop) >>= return . DecafBinArithOp

addop = (opadd >> return DecafPlusOp)
subop = (opmin >> return DecafMinOp)
mulop = (opmul >> return DecafMulOp)
divop = (opdiv >> return DecafDivOp)
modop = (opmod >> return DecafModOp)

relop = (ltop <|> gtop <|> lteop <|> gteop) >>= return . DecafBinRelOp

ltop = (oplt >> return DecafLTOp)
gtop = (opgt >> return DecafGTOp)
lteop = (oplte >> return DecafLTEOp)
gteop = (opgte >> return DecafGTEOp)

eqop = (oeq <|> oneq) >>= return . DecafBinEqOp

oeq = (opeq >> return DecafEqOp)
oneq = (opneq >> return DecafNEqOp)

condop = (andop <|> orop) >>= return . DecafBinCondOp

andop = (opand >> return DecafAndOp)
orop = (opor >> return DecafOrOp)

assignop = aseq
        <|> mineq
        <|> pluseq

aseq = (assign >> return DecafEq)
mineq = (minusassign >> return DecafMinusEq)
pluseq = (plusassign >> return DecafPlusEq)

lit = ilit <|> clit  <|> blit
slit = (strlit >>= return . DecafString)
ilit = (int >>= return . DecafIntLit)
blit = (boollit >>= return . DecafBoolLit)
clit = (chrlit >>= return . DecafCharLit . DecafCharacter)

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
