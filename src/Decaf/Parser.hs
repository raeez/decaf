module Decaf.Parser
where
import Text.ParserCombinators.Parsec
import Decaf.Tokens
import Decaf.AST
import Decaf.Scanner
import Decaf.Util

-- |DecafParser defines a Parser type for DecafToken
type DecafParser a = GenParser Token () a

morphPos :: SourcePos -> DecafPosition
morphPos pos = (sourceLine pos, sourceColumn pos)

decafToken :: (DecafToken -> Maybe a) -> DecafParser a
decafToken test = token showToken posToken testToken
                  where
                    showToken (_, t) = show t
                    posToken ((p1, _), _) = p1
                    testToken (_, t) = test t

identf :: String -> DecafParser String
identf name = decafToken (\tok -> case tok of
                              Identf n -> if n == name
                                          then Just name
                                          else Nothing
                              _        -> Nothing)

varident :: DecafParser String
varident = decafToken (\tok -> case tok of
                            Identf name -> Just name
                            _           -> Nothing)

reserv :: String -> DecafParser String
reserv name = decafToken (\tok -> case tok of
                                  Reserv s | s == name -> Just name
                                  _                    -> Nothing)

strlit :: DecafParser String
strlit = decafToken (\tok -> case tok of
                          StrLit s -> Just s
                          _        -> Nothing)

int :: DecafParser DecafInteger
int = fmap DecafDec declit <|> fmap DecafHex hexlit

hexlit :: DecafParser String
hexlit = decafToken (\tok -> case tok of
                          HexLit s -> Just s
                          _        -> Nothing)

declit :: DecafParser String
declit = decafToken (\tok -> case tok of
                            DecLit s -> Just s
                            _        -> Nothing)

chrlit :: DecafParser Char
chrlit = decafToken (\tok -> case tok of
                            CharLit c -> Just c
                            _         -> Nothing)

boollit :: DecafParser Bool
boollit = decafToken (\tok -> case tok of
                            BoolLit True  -> Just True
                            BoolLit False -> Just False
                            _             -> Nothing)

lparen :: DecafParser ()
lparen = decafToken (\tok -> case tok of
                          LParen -> Just ()
                          _      -> Nothing)

rparen :: DecafParser ()
rparen = decafToken (\tok -> case tok of
                          RParen -> Just ()
                          _      -> Nothing)

lbrace :: DecafParser ()
lbrace = decafToken (\tok -> case tok of
                          LBrace -> Just ()
                          _      -> Nothing)

rbrace :: DecafParser ()
rbrace = decafToken (\tok -> case tok of
                          RBrace -> Just ()
                          _      -> Nothing)

lbrack :: DecafParser ()
lbrack = decafToken (\tok -> case tok of
                            LBrack -> Just ()
                            _      -> Nothing)

rbrack :: DecafParser ()
rbrack = decafToken (\tok -> case tok of
                            RBrack -> Just ()
                            _      -> Nothing)

comma :: DecafParser ()
comma = decafToken (\tok -> case tok of
                            Comma -> Just ()
                            _     -> Nothing)

semi :: DecafParser ()
semi = decafToken (\tok -> case tok of
                        Semi -> Just ()
                        _    -> Nothing)

opand :: DecafParser ()
opand = decafToken (\tok -> case tok of
                          OpAnd -> Just ()
                          _     -> Nothing)
opor :: DecafParser ()
opor = decafToken (\tok -> case tok of
                          OpOr -> Just ()
                          _    -> Nothing)

opeq :: DecafParser ()
opeq = decafToken (\tok -> case tok of
                          OpEq -> Just ()
                          _    -> Nothing)

opneq :: DecafParser ()
opneq = decafToken (\tok -> case tok of
                          OpNEq -> Just ()
                          _     -> Nothing)
                            
oplt :: DecafParser ()
oplt = decafToken (\tok -> case tok of
                          OpLT -> Just ()
                          _    -> Nothing)

opgt :: DecafParser ()
opgt = decafToken (\tok -> case tok of
                          OpGT -> Just ()
                          _    -> Nothing)

oplte :: DecafParser ()
oplte = decafToken (\tok -> case tok of
                          OpLTE-> Just ()
                          _    -> Nothing)

opgte :: DecafParser ()
opgte = decafToken (\tok -> case tok of
                          OpGTE-> Just ()
                          _    -> Nothing)

opadd :: DecafParser ()
opadd = decafToken (\tok -> case tok of
                          OpAdd-> Just ()
                          _    -> Nothing)

opmin :: DecafParser ()
opmin = decafToken (\tok -> case tok of
                          OpMin-> Just ()
                          _    -> Nothing)

opmul :: DecafParser ()
opmul = decafToken (\tok -> case tok of
                          OpMul -> Just ()
                          _     -> Nothing)

opdiv :: DecafParser ()
opdiv = decafToken (\tok -> case tok of
                          OpDiv -> Just ()
                          _     -> Nothing)

opmod :: DecafParser ()
opmod = decafToken (\tok -> case tok of
                          OpMod -> Just ()
                          _     -> Nothing)

opnot :: DecafParser ()
opnot = decafToken (\tok -> case tok of
                          OpNot -> Just ()
                          _     -> Nothing)

assign :: DecafParser ()
assign = decafToken (\tok -> case tok of
                          Assign -> Just ()
                          _      -> Nothing)

plusassign :: DecafParser ()
plusassign = decafToken (\tok -> case tok of
                          PlusAssign -> Just ()
                          _          -> Nothing)

minusassign :: DecafParser ()
minusassign = decafToken (\tok -> case tok of
                          MinusAssign -> Just ()
                          _           -> Nothing)

-- Parser interface
containsErrors :: Maybe a -> Bool
containsErrors (Just _) = False
containsErrors Nothing = True

parser :: String -> String
parser i = getReport $ ps program i

parseToksProgram :: [Token] -> Report DecafProgram
parseToksProgram intokstream = ps_ program intokstream
                               where ps_ p s = case parse p "decaf-parser" s of
                                       Left err -> RError $ show err
                                       Right val -> RSuccess val

ps :: DecafParser a -> String -> Report a
ps p i = case parse p "decaf-parser" (eatFirst i) of
          Left err  -> RError $ show err
          Right val -> RSuccess val

qs :: DecafParser a -> String -> Maybe a
qs p i = case parse p "internal-decaf-parser" (eatFirst i) of
        Left _    -> Nothing
        Right val -> Just val

program :: DecafParser DecafProgram
program = do
            p <- getPosition
            reserv "class" >> identf "Program"
            lbrace
            f <- many (try fielddecl)
            m <- many methoddecl
            rbrace
            let p' = morphPos p
            return $ DecafProgram (concat  f) m p'

fielddecl :: DecafParser [DecafField]
fielddecl = do
              t <- vartype
              fd <- fdecl t `sepBy` comma
              semi
              return fd

fdecl :: DecafType -> DecafParser DecafField
fdecl t = try (adecl t) <|> vdecl t

vdecl :: DecafType -> DecafParser DecafField
vdecl t = do
            p <- getPosition
            i <- identvar
            let p' = morphPos p
            return $ DecafVarField (DecafVar t i p') p'

adecl :: DecafType -> DecafParser DecafField
adecl t = do
            p <- getPosition
            i <- identvar
            lbrack
            s <- int
            rbrack
            let p' = morphPos p
            return $ DecafArrField (DecafArr t i s p') p'

methoddecl :: DecafParser DecafMethod
methoddecl = do
              p <- getPosition
              t <- rettype
              i <- identvar
              lparen
              ps <- pdecl `sepBy` comma
              rparen
              b <- block
              let p' = morphPos p
              return $ DecafMethod t i ps b p'

pdecl :: DecafParser DecafVar
pdecl = do
          p <- getPosition
          t <- vartype
          i <- identvar
          let p' = morphPos p
          return $ DecafVar t i p'

block :: DecafParser DecafBlock
block = do
          p <- getPosition
          lbrace
          v <- many (try vardecl)
          s <- many statement
          rbrace
          let p' = morphPos p
          return $ DecafBlock (concat v) s p'

vardecl :: DecafParser [DecafVar]
vardecl = do
            p <- getPosition
            t <- vartype
            i <- identvar `sepBy1` comma
            semi
            let p' = morphPos p
                i' = map (($p') . DecafVar t) i
            return i'

identvar :: DecafParser DecafIdentifier
identvar = varident

rettype, vartype :: DecafParser DecafType
vartype = integertype <|> booleantype
rettype = vartype <|> voidtype

voidtype, booleantype, integertype :: DecafParser DecafType
voidtype = reserv "void" >> return DecafVoid
integertype = reserv "int" >> return DecafInteger
booleantype = reserv "boolean" >> return DecafBoolean

statement :: DecafParser DecafStm
statement =  try (do
               p <- getPosition
               l <- location
               o <- assignop
               e <- expr
               semi
               let p' = morphPos p
               return $ DecafAssignStm l o e p')
         <|> try (do
               p <- getPosition
               m <- methodcall
               semi
               let p' = morphPos p
               return $ DecafMethodStm m p')
         <|> (do
               p <- getPosition
               reserv "if"
               lparen
               e <- expr
               rparen
               b1 <- block
               b <- mayb (reserv "else" >> block)
               let p' = morphPos p
               return $ DecafIfStm e b1 b p')
         <|> (do
                p <- getPosition
                reserv "for"
                i <- identvar
                aseq
                e1 <- expr
                comma
                e2 <- expr
                b <- block
                let p' = morphPos p
                return $ DecafForStm i e1 e2 b p')
         <|> (do
                p <- getPosition
                reserv "return"
                e <- mayb expr
                semi
                let p' = morphPos p
                return $ DecafRetStm e p')
         <|> (reserv "break" >> semi >> fmap (DecafBreakStm . morphPos) getPosition)
         <|> (reserv "continue" >> semi >> fmap (DecafContStm . morphPos) getPosition)
         <|> (block >>= \o -> fmap (DecafBlockStm o . morphPos) getPosition)

methodcall :: DecafParser DecafMethodCall
methodcall = (do
                p <- getPosition
                reserv "callout"
                lparen
                s <- slit
                comma <|> return ()
                a <- calloutarg `sepBy` comma
                rparen
                let p' = morphPos p
                return $ DecafMethodCallout s a p')
          <|> (do
                p' <- getPosition
                i <- identvar
                lparen
                p <- expr `sepBy` comma
                rparen
                let p'' = morphPos p'
                return $ DecafPureMethodCall i p p'')

-- |'rewriteExpr' rewrites a parsed concrete expression tree into an abstract syntax expression tree
-- This function utilizes the recursive rewriteExprTail, rewriteTerm, rewriteTermTail and rewriteFactor to rewrite the entire tree in-place.
rewriteExpr :: DecafExpr -> DecafExpr
rewriteExpr (DecafExpr term EmptyExpr' _)
  = rewriteTerm term

rewriteExpr (DecafExpr term (Expr' binop term' EmptyExpr' p) _)
  = DecafBinExpr (rewriteTerm term) binop (rewriteTerm term') p

rewriteExpr (DecafExpr term (Expr' binop term' expr'@(Expr' binop' _ _ p) p') _)
  = DecafBinExpr (rewriteTerm term) binop  (DecafBinExpr (rewriteTerm term') binop' (rewriteExprTail expr') p') p

rewriteExpr e@(DecafLocExpr _ _)     = e
rewriteExpr e@(DecafMethodExpr _ _)  = e
rewriteExpr e@(DecafLitExpr _ _)     = e
rewriteExpr e@(DecafBinExpr _ _ _ _) = e
rewriteExpr e@(DecafNotExpr _ _)     = e
rewriteExpr e@(DecafMinExpr _ _)     = e
rewriteExpr e@(DecafParenExpr _ _)   = e

-- |'rewriteExprTail'
rewriteExprTail :: Expr' -> DecafExpr
rewriteExprTail (Expr' _ term expr@(Expr' binop _ _ p) _)
  = DecafBinExpr (rewriteTerm term) binop (rewriteExprTail expr) p

rewriteExprTail (Expr' _ term EmptyExpr' _)
  = rewriteTerm term

rewriteExprTail EmptyExpr'
  = error "Parser.hs:358 invalid concrete expr tree; rewriteExprTail encountered an EmptyTerm'"

rewriteTerm :: Term -> DecafExpr
rewriteTerm (Term factor EmptyTerm' _)
  = rewriteFactor factor
rewriteTerm (Term factor (Term' binop factor' EmptyTerm' p) _)
  = DecafBinExpr (rewriteFactor factor) binop (rewriteFactor factor') p
rewriteTerm (Term factor (Term' binop factor' term'@(Term' binop' _ _ p') p) _)
  = DecafBinExpr (rewriteFactor factor) binop  (DecafBinExpr (rewriteFactor factor') binop' (rewriteTermTail term') p') p

rewriteTermTail :: Term' -> DecafExpr
rewriteTermTail (Term' _ factor term@(Term' binop _ _ p) _)
  = DecafBinExpr (rewriteFactor factor) binop (rewriteTermTail term) p

rewriteTermTail (Term' _ factor EmptyTerm' _)
  = rewriteFactor factor

rewriteTermTail (EmptyTerm')
  = error "Parser.hs:366 invalid concrete expr tree; rewriteTermTail encountered an EmptyTerm'"

rewriteFactor :: Factor -> DecafExpr
rewriteFactor (DecafParenExpr' expr p)
  = DecafParenExpr (rewriteExpr expr) p

rewriteFactor (DecafNotExpr' expr p)
  = DecafNotExpr (rewriteExpr expr) p

rewriteFactor (DecafMinExpr' expr p)
  = DecafMinExpr (rewriteExpr expr) p

rewriteFactor (DecafLocExpr' loc p)
  = DecafLocExpr loc p

rewriteFactor (DecafMethodExpr' meth p)
  = DecafMethodExpr meth p

rewriteFactor (DecafLitExpr' lit p)
  = DecafLitExpr lit p

mayb :: DecafParser a -> DecafParser (Maybe a)
mayb p = fmap Just p <|> return Nothing

-- left associative, right recursive
expr :: DecafParser DecafExpr
expr = do
        t <- term
        e <- expr'
        fmap (rewriteExpr . DecafExpr t e . morphPos) getPosition

expr' :: DecafParser Expr'
expr' = do
          b <- toplevelOp
          t <- term
          e <- expr'
          fmap (Expr' b t e . morphPos) getPosition
     <|> return EmptyExpr'

term :: DecafParser Term
term = do
         f <- factor
         t <- term'
         fmap (Term f t . morphPos) getPosition

term' :: DecafParser Term'
term' = do
          b <- botlevelOp
          f <- factor
          t <- term'
          fmap (Term' b f t . morphPos) getPosition
     <|> return EmptyTerm'

factor :: DecafParser Factor
factor = (try methodcall >>= \o -> fmap (DecafMethodExpr' o . morphPos) getPosition)
      <|> (location >>= \o -> fmap (DecafLocExpr' o . morphPos) getPosition)
      <|> (lit >>= \o -> fmap (DecafLitExpr' o . morphPos) getPosition)
      <|> (opnot >> expr >>= \o -> fmap (DecafNotExpr' o . morphPos) getPosition)
      <|> do
            opmin
            e <- expr
            p <- getPosition
            let p' = morphPos p
            return $ DecafMinExpr' (case e of
                        DecafLitExpr (DecafIntLit (DecafHex int) _) _ -> DecafLitExpr (DecafIntLit (DecafHex ("-" ++ int)) p') p'
                        DecafLitExpr (DecafIntLit (DecafDec int) _) _ -> DecafLitExpr (DecafIntLit (DecafDec ("-" ++ int)) p') p'
                        e -> e) p'
      <|> do
            lparen
            e <- expr
            rparen
            fmap (DecafParenExpr' e . morphPos) getPosition

toplevelOp :: DecafParser DecafBinOp
toplevelOp = toparithop <|> relop <|> eqop <|> condop

botlevelOp :: DecafParser DecafBinOp
botlevelOp = botarithop

toparithop :: DecafParser DecafBinOp
toparithop = (addop <|> subop) >>= \o -> fmap (DecafBinArithOp o . morphPos) getPosition

botarithop :: DecafParser DecafBinOp
botarithop = (mulop <|> divop <|> modop) >>= \o -> fmap (DecafBinArithOp o . morphPos) getPosition

addop, subop, mulop, divop, modop :: DecafParser DecafArithOp
addop = opadd >> fmap (DecafPlusOp . morphPos) getPosition
subop = opmin >> fmap (DecafMinOp . morphPos) getPosition
mulop = opmul >> fmap (DecafMulOp . morphPos) getPosition
divop = opdiv >> fmap (DecafDivOp . morphPos) getPosition
modop = opmod >> fmap (DecafModOp . morphPos) getPosition

relop :: DecafParser DecafBinOp
relop = (ltop <|> gtop <|> lteop <|> gteop) >>= \o -> fmap (DecafBinRelOp o . morphPos) getPosition

ltop, gtop, lteop, gteop :: DecafParser DecafRelOp
ltop = oplt >> fmap (DecafLTOp . morphPos) getPosition
gtop = opgt >> fmap (DecafGTOp . morphPos) getPosition
lteop = oplte >> fmap (DecafLTEOp . morphPos) getPosition
gteop = opgte >> fmap (DecafGTEOp . morphPos) getPosition

eqop :: DecafParser DecafBinOp
eqop = (oeq <|> oneq) >>= \o -> fmap (DecafBinEqOp o . morphPos) getPosition

oeq, oneq :: DecafParser DecafEqOp
oeq = opeq >> fmap (DecafEqOp . morphPos) getPosition
oneq = opneq >> fmap (DecafNEqOp . morphPos) getPosition

condop :: DecafParser DecafBinOp
condop = (andop <|> orop) >>= \o -> fmap (DecafBinCondOp o . morphPos) getPosition

andop, orop :: DecafParser DecafCondOp
andop = opand >> fmap (DecafAndOp . morphPos) getPosition
orop = opor >> fmap (DecafOrOp . morphPos) getPosition

assignop, aseq, mineq, pluseq :: DecafParser DecafAssignOp
assignop = aseq <|> mineq <|> pluseq
aseq = assign >> fmap (DecafEq . morphPos) getPosition
mineq = minusassign >> fmap (DecafMinusEq . morphPos) getPosition
pluseq = plusassign >> fmap (DecafPlusEq . morphPos) getPosition

lit :: DecafParser DecafLiteral
lit = ilit <|> clit  <|> blit

slit :: DecafParser DecafString
slit = strlit

ilit :: DecafParser DecafLiteral
ilit = int >>= \i -> fmap (DecafIntLit i . morphPos) getPosition

blit :: DecafParser DecafLiteral
blit = boollit >>= \b -> fmap (DecafBoolLit b . morphPos) getPosition

clit :: DecafParser DecafLiteral
clit = chrlit >>= \c -> fmap (DecafCharLit c . morphPos) getPosition

location :: DecafParser DecafLoc
location = try arrlocation <|> varlocation

varlocation :: DecafParser DecafLoc
varlocation = identvar >>= \i -> fmap (DecafVarLoc i . morphPos) getPosition

arrlocation :: DecafParser DecafLoc
arrlocation = do
                p <- getPosition
                i <- identvar
                lbrack
                e <- expr
                rbrack
                let p' = morphPos p
                return $ DecafArrLoc i e p'

calloutarg :: DecafParser DecafCalloutArg
calloutarg = (expr >>= (\o -> fmap (DecafCalloutArgExpr o . morphPos) getPosition))
          <|> (slit >>= (\o -> fmap (DecafCalloutArgStr o . morphPos) getPosition))
