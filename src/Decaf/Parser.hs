module Decaf.Parser
where
import Text.ParserCombinators.Parsec
import Decaf.Tokens
import Decaf.AST
import Decaf.Scanner

-- |DecafParser defines a Parser type for DecafToken
type DecafParser a = GenParser Token () a

morphPos :: SourcePos -> DecafPosition
morphPos pos = (sourceLine pos, sourceColumn pos)

decafToken :: (DecafToken -> Maybe a) -> DecafParser a
decafToken test = token showToken posToken testToken
                  where
                    showToken (_, t) = show t
                    posToken ((p1, p2), _) = p1
                    testToken (_, t) = test t

identf name = decafToken (\tok -> case tok of
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


-- Parser interface

containsErrors (Just a)= False
containsErrors Nothing = True

parser i = getReport $ ps program i

-- try to find an interface to testSemanticsChecker
-- will use parseStringProgram  -J
--
parseToksProgram :: [Token] -> Report DecafProgram
parseToksProgram intokstream = ps_ program intokstream
                               where ps_ p s = case parse p "decaf-parser" s of
                                       Left err -> RError $ show err
                                       Right val -> RSuccess val

ps p i = case parse p "decaf-parser" (eatFirst i) of
          Left err -> RError $ show err
          Right val -> RSuccess val

qs p i = case parse p "internal-decaf-parser" (eatFirst i) of
        Left err -> Nothing
        Right val -> Just val

program = (do
            reserv "class" >> identf "Program"
            lbrace
            f <- many $ (try $ fielddecl)
            m <- many $ ( methoddecl)
            rbrace
            p <- getPosition
            let p' = morphPos p
            return $ DecafProgram (concat  f) m p')

fielddecl = (do
              t <- vartype
              fd <- (fdecl t) `sepBy` comma
              semi
              p <- getPosition
              return fd)

fdecl t = (try $ adecl t) <|> (vdecl t)

vdecl t = do
            i <- identvar
            p <- getPosition
            let p' = morphPos p
            return $ DecafVarField (DecafVar t i p') p'

adecl t = do
            i <- identvar
            lbrack
            s <- int
            rbrack
            p <- getPosition
            let p' = morphPos p
            return $ DecafArrField (DecafArr t i s p') p'

methoddecl = (do
              t <- rettype
              i <- identvar
              lparen
              p <- pdecl `sepBy` comma
              rparen
              b <- block
              p' <- getPosition
              let p'' = morphPos p'
                  mappedP = map ($p'') p
              return $ DecafMethod t i mappedP b p'')

pdecl = do
          t <- vartype
          i <- identvar
          return $ DecafVar t i

block = do
          lbrace
          v <- many (try vardecl)
          s <- many statement
          rbrace
          p <- getPosition
          let p' = morphPos p
          return $ DecafBlock (concat v) s p'

vardecl = do
            t <- vartype
            i <- identvar `sepBy1` comma
            semi
            p <- getPosition
            let p' = morphPos p
                i' = map ($p') (map  (DecafVar t) i)
            return $ i'


identvar = (varident >>= return)



voidtype = (reserv "void" >> return DecafVoid)

integertype = (reserv "int" >> return DecafInteger)
booleantype = (reserv "boolean" >> return DecafBoolean)
vartype = integertype <|> booleantype
rettype = vartype <|> voidtype

statement =  try (do
               l <- location
               o <- assignop
               e <- expr
               semi
               p <- getPosition
               let p' = morphPos p
               return $ DecafAssignStm l (o p') e p')
         <|> try (do
               m <- methodcall
               semi
               p <- getPosition
               let p' = morphPos p
               return $ DecafMethodStm m p')
         <|> (do
               reserv "if"
               lparen
               e <- expr
               rparen
               b1 <- block
               b <- mayb (reserv "else" >> block)
               p <- getPosition
               let p' = morphPos p
               return $ DecafIfStm e b1 b p')
         <|> (do
                reserv "for"
                i <- identvar
                aseq
                e1 <- expr
                comma
                e2 <- expr
                b <- block
                p <- getPosition
                let p' = morphPos p
                return $ DecafForStm i e1 e2 b p')
         <|> (do
                reserv "return"
                e <- mayb expr
                semi
                p <- getPosition
                let p' = morphPos p
                return $ DecafRetStm e p')
         <|> (reserv "break" >> semi >> getPosition >>= return . DecafBreakStm . morphPos)
         <|> (reserv "continue" >> semi >> getPosition >>= return . DecafContStm . morphPos)
         <|> (block >>= \o -> getPosition >>= return . DecafBlockStm o . morphPos)

methodcall = (do
                reserv "callout"
                lparen
                s <- slit
                comma <|> (return ())
                a <- calloutarg `sepBy` comma
                rparen
                p <- getPosition
                let p' = morphPos p
                return $ DecafMethodCallout s a p')
          <|> (do
                i <- identvar
                lparen
                p <- expr `sepBy` comma
                rparen
                p' <- getPosition
                let p'' = morphPos p'
                return $ DecafPureMethodCall i p p'')

-- |'rewriteExpr' rewrites a parsed concrete expression tree into an abstract syntax expression tree
-- This function utilizes the recursive rewriteExprTail, rewriteTerm, rewriteTermTail and rewriteFactor to rewrite the entire tree in-place.
rewriteExpr :: DecafExpr -> DecafExpr
rewriteExpr (DecafExpr term (EmptyExpr') _)
  = rewriteTerm term

rewriteExpr (DecafExpr term (Expr' binop term' (EmptyExpr') p) _)
  = DecafBinExpr (rewriteTerm term) binop (rewriteTerm term') p

rewriteExpr (DecafExpr term (Expr' binop term' expr'@(Expr' binop' _ _ p) p') _)
  = DecafBinExpr (rewriteTerm term) binop  (DecafBinExpr (rewriteTerm term') binop' (rewriteExprTail expr') p') p

rewriteExpr e@(DecafLocExpr _ _)           = e
rewriteExpr e@(DecafMethodExpr _ _)       = e
rewriteExpr e@(DecafLitExpr _ _)           = e
rewriteExpr e@(DecafBinExpr _ _ _ _) = e
rewriteExpr e@(DecafNotExpr _ _)          = e
rewriteExpr e@(DecafMinExpr _ _)          = e
rewriteExpr e@(DecafParenExpr _ _)      = e

-- |'rewriteExprTail'
rewriteExprTail :: Expr' -> DecafExpr
rewriteExprTail (Expr' _ term expr@(Expr' binop _ _ p) _)
  = DecafBinExpr (rewriteTerm term) binop (rewriteExprTail expr) p

rewriteExprTail (Expr' _ term (EmptyExpr') _)
  = rewriteTerm term

rewriteTerm :: Term -> DecafExpr
rewriteTerm (Term factor (EmptyTerm') _)                                         = rewriteFactor factor
rewriteTerm (Term factor (Term' binop factor' (EmptyTerm') p) _)              = DecafBinExpr (rewriteFactor factor) binop (rewriteFactor factor') p
rewriteTerm (Term factor (Term' binop factor' term'@(Term' binop' _ _ p') p) _)   = DecafBinExpr (rewriteFactor factor) binop  (DecafBinExpr (rewriteFactor factor') binop' (rewriteTermTail term') p') p

rewriteTermTail :: Term' -> DecafExpr
rewriteTermTail (Term' _ factor term@(Term' binop _ _ p) _)               = DecafBinExpr (rewriteFactor factor) binop (rewriteTermTail term) p
rewriteTermTail (Term' _ factor (EmptyTerm') p)                           = rewriteFactor factor

rewriteFactor :: Factor -> DecafExpr
rewriteFactor (DecafParenExpr' expr p)  = DecafParenExpr (rewriteExpr expr) p
rewriteFactor (DecafNotExpr' expr p)    = DecafNotExpr (rewriteExpr expr) p
rewriteFactor (DecafMinExpr' expr p)    = DecafMinExpr (rewriteExpr expr) p
rewriteFactor (DecafLocExpr' loc p)     = DecafLocExpr loc p
rewriteFactor (DecafMethodExpr' meth p) = DecafMethodExpr meth p
rewriteFactor (DecafLitExpr' lit p)     = DecafLitExpr lit p


-- left associative, right recursive

mayb p = do
          (p >>= return . Just) <|> (return Nothing)

expr = do
        t <- term
        e <- expr'
        getPosition >>= return . rewriteExpr . DecafExpr t e . morphPos
        --return $ DecafExpr t e

expr' = (do
          b <- toplevelOp
          t <- term
          e <- expr'
          getPosition >>= return . Expr' b t e . morphPos)
     <|> (return $ EmptyExpr')

term = do
         f <- factor
         t <- term'
         getPosition >>= return . Term f t . morphPos

term' = (do
          b <- botlevelOp
          f <- factor
          t <- term'
          getPosition >>= return . Term' b f t . morphPos)
     <|> (return $ EmptyTerm')

factor = (try methodcall >>= \o -> getPosition >>= return . DecafMethodExpr' o . morphPos)
      <|> (location >>= \o -> getPosition >>= return . DecafLocExpr' o . morphPos)
      <|> (lit >>= \o -> getPosition >>= return . DecafLitExpr' o . morphPos)
      <|> (opnot >> expr >>= \o -> getPosition >>= return . DecafNotExpr' o . morphPos)
      <|> (opmin >> expr >>= \o -> getPosition >>= return . DecafMinExpr' o . morphPos)
      <|> (do
            lparen
            e <- expr
            rparen
            getPosition >>= return . DecafParenExpr' e . morphPos)

toplevelOp = ((addop <|> subop) >>= \o -> getPosition >>= return . DecafBinArithOp o . morphPos) <|> relop <|> eqop <|> condop
botlevelOp = (mulop <|> divop <|> modop) >>= \o -> getPosition >>= return . DecafBinArithOp o . morphPos

addop = (opadd >> getPosition >>= return . DecafPlusOp . morphPos)
subop = (opmin >> getPosition >>= return . DecafMinOp . morphPos)
mulop = (opmul >> getPosition >>= return . DecafMulOp . morphPos)
divop = (opdiv >> getPosition >>= return . DecafDivOp . morphPos)
modop = (opmod >> getPosition >>= return . DecafModOp . morphPos)

relop = (ltop <|> gtop <|> lteop <|> gteop) >>= \o -> getPosition >>= return . DecafBinRelOp o . morphPos

ltop = (oplt >> getPosition >>= return . DecafLTOp . morphPos)
gtop = (opgt >> getPosition >>= return . DecafGTOp . morphPos)
lteop = (oplte >> getPosition >>=  return . DecafLTEOp . morphPos)
gteop = (opgte >> getPosition >>= return . DecafGTEOp . morphPos)

eqop = (oeq <|> oneq) >>= (\o -> getPosition >>= return . DecafBinEqOp o . morphPos)
condop = (andop <|> orop) >>= (\o -> (do
                                        p <- getPosition
                                        let p' = morphPos p
                                        return $ DecafBinCondOp (o p') p'))

oeq = (opeq >> getPosition >>= return . DecafEqOp . morphPos)
oneq = (opneq >> getPosition >>= return . DecafNEqOp . morphPos)

andop = (opand >> return DecafAndOp)
orop = (opor >> return DecafOrOp)

assignop = aseq
        <|> mineq
        <|> pluseq

aseq = (assign >> return DecafEq)
mineq = (minusassign >> return DecafMinusEq)
pluseq = (plusassign >> return DecafPlusEq)

lit = ilit <|> clit  <|> blit
slit = (strlit >>= return)
ilit = (int >>= \i -> getPosition >>= return . DecafIntLit i . morphPos)
blit = (boollit >>= \b -> getPosition >>= return . DecafBoolLit b . morphPos)
clit = (chrlit >>= \c -> getPosition >>= return . DecafCharLit c . morphPos)

location = (try arrlocation) <|> varlocation

varlocation = do
                i <- identvar
                getPosition >>= (return . DecafVarLoc i . morphPos)

arrlocation = do
                i <- identvar
                lbrack
                e <- expr
                rbrack
                getPosition >>= (return . DecafArrLoc i e . morphPos)

calloutarg = (expr >>= (\o -> getPosition >>= (return . DecafCalloutArgExpr o . morphPos)))
          <|> (slit >>= (\o -> getPosition >>= (return . DecafCalloutArgStr o . morphPos)))
