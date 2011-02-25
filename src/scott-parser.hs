module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Pos
import System.Exit
import Monad
import Data.List
import Scanner


-- token parser def
type TokenParser a = GenParser Token () a

mytoken :: (Tok -> Maybe a) -> TokenParser a
mytoken test = token showToken posToken testToken
    where
      showToken (pos,tok)   = show tok
      posToken  (pos,tok)   = pos
      testToken (pos,tok)   = test tok


{- primitive parser terminals -}

did
    = mytoken (\tok -> case tok of 
                         Identifier name -> Just name
                         other           -> Nothing)
didname name
    = mytoken (\tok -> case tok of 
                         Identifier n | n == name -> Just ()
                         other           -> Nothing)
res name
    = mytoken (\tok -> case tok of
                         Scanner.Reserved s | s == name  -> Just name
                         other          -> Nothing)
int
    = mytoken (\tok -> case tok of 
                         Intliteral val -> Just val
                         other -> Nothing)
char
    = mytoken (\tok -> case tok of 
                         Charliteral val -> Just val
                         other -> Nothing)
bool
    = mytoken (\tok -> case tok of 
                         Booleanliteral val -> Just val
                         other -> Nothing)
string
    = mytoken (\tok -> case tok of 
                         Stringliteral val -> Just val
                         other -> Nothing)
errortok
    = mytoken (\tok -> case tok of 
                         Error val -> Just val
                         other -> Nothing)

{- grammar data types -}

data Program = Program { fields :: [FieldDec],
                         methods :: [MethodDec] }

data FieldDec = FieldDec { fieldtype :: DType,
                           fielddecs :: [GenVarID]}

-- might need to change [VarDec] to [SingleVarDec]
data MethodDec = MethodDec { rt :: ReturnType, methodname :: Did, 
                             parameters :: [SingleVar], body :: Block}

data Block = Block { blockdecs :: [VarDec], 
                     blockstatements :: [Statement] }

data VarDec = VarDec { vartype :: DType, 
                       vardecs :: [Did] }

data SingleVar = SingleVar (DType, Did)

data DType = DInteger | DBoolean 

data Statement = AssignmentStatement Assignment
               | MethodCallStatement MethodCall
               | Callout (String, [CalloutArg])   -- args optional
               | If (Expression, Block, Block)  -- second block optional
               | For (Did, Expression, Expression, Block)
               | Return Expression
               | Break
               | Continue
               | BlockSt Block

data MethodCall = MethodCall (Did, [Expression])
data Assignment = Assignment (Location, AssignOp, Expression)

data AssignOp = Eq | Pleq | Mineq

-- already did methodcall
-- already did methodname

type Location = GenVarID

{-data Expression = LocExp Location
                | CallExp MethodCall
                | LiteralExp Literal
                | OpExp (Expression, BinOp, Expression)
                | UnExp (UnaryOp, Expression)
                | ParenExp Expression
                | VoidExp -- returned by void functions
-}
data Expression = Expression' Expression' [(BinOp, Expression')]
                | VoidExp

data Expression' = TermExpr TermExpr
                 | ParenExpr Expression
                 | NotExpr Expression'
                 | MinExpr Expression'

data TermExpr = LocExpr Location
              | CallExpr MethodCall
              | LitExpr Literal


data CalloutArg = ExpArg Expression 
                | StrArg String

data BinOp = ArithBin ArithOp
           | RelBin RelOp
           | EqBin EqOp
           | CondBin CondOp

data ArithOp = Plus | Minus | Times | Divide | Mod
data RelOp = Less | Greater | LessEq | GreaterEq
data EqOp = EqEq | NotEq
data CondOp = And | Or

data UnaryOp = Negative | Not

data Literal = IntLiteral String
             | CharLiteral String
             | BoolLiteral String
             | StringLiteral String

type Did = String 


type Size = String
data ReturnType = PrimitiveType DType
                | Void
data GenVarID = Basic Did 
           | Array (Did, Size)


-- hacked types

data Aom = AssAOM Assignment | MethAOM MethodCall
data Fom = FieldFOM FieldDec | MethFOM MethodDec


comma = res ","
semicolon = res ";"


-- utilities for hacked program grammar
isField entry = case entry of
                  FieldFOM d -> True
                  MethFOM d -> False

fomtoField x = case x of
                 FieldFOM y -> y
fomtoMeth x  = case x of
                 MethFOM y -> y


{- All Primary Parsers -}

program = do (res "class") >> (didname "Program") >> (res "{") 
             entries <- many fom
             res "}"
             return $ Program (map fomtoField (filter isField entries)) (map fomtoMeth (filter (not.isField) entries))



-- field or method dec
-- try used only to look ahead one token (since both start with ID)
fom = (do res "void"; methodDec Void >>= return.MethFOM) <|> 
      (do i <- dtype
          ((try (methodDec $PrimitiveType i) >>= return.MethFOM) <|> (fieldDec i >>= return.FieldFOM)))

-- type passed in due to left-factoring
fieldDec t = do f <- sepBy1 genvarid comma
                semicolon <?> "semicolon"
                return $FieldDec t f

-- type passed in 
-- (uses try to look ahead one token for '('
-- if not found, tries fieldDec
methodDec rt = do i <- did
                  res "("
                  params <- sepBy singleVar comma
                  res ")"
                  body <- block
                  return $MethodDec rt i params body               

block = do res "{"
           ds <- many varDec
           st <- many statement
           res "}"
           return $Block ds st

varDec = do t <- dtype
            v <- sepBy1 did comma
            semicolon
            return $VarDec t v

singleVar = do t <- dtype
               v <- did
               return $SingleVar (t,v)

dType = (res "int" >> return DInteger) 
        <|> (res "boolean" >> return DBoolean)

callout = do res "callout"; res "("; n <- Main.string; 
             args <- (do comma; sepBy calloutArg comma) <|> (return []); 
             res ")"; return $Callout (n, args)

calloutArg :: TokenParser CalloutArg
calloutArg = (expression >>= return.ExpArg) <|> (Main.string >>= return.StrArg)


-- try needed because loc and methodCall start with identifiers
stparsers = [ do s <- assormeth;
                 case s of
                   AssAOM s -> return $AssignmentStatement s
                   MethAOM s -> return $MethodCallStatement s
            , do c <- callout; semicolon; return c
            , do res "if"; res "("; cond <- expression; res ")"; t <- block; 
                 (try (do res "else"; t2 <- block; return $If (cond, t, t2))) <|> (return $If(cond, t, (Block [] [])))
            , do res "for"; i <- did; res "="; e <- expression; 
                 comma; e2 <- expression; b <- block; return $For (i, e, e2, b)
            , do res "return"; (try (do e <- expression; semicolon; return $Return e)) <|> (semicolon >> return (Return VoidExp))
            , res "break" >> semicolon >> return Break
            , res "continue" >> semicolon >> return Continue
            , do b <- block; return $BlockSt b
            ]

assormeth = do idd <- genvarid;
               case idd of
                 Basic id ->
                     (do op <- assignOp; ex <- expression; semicolon;
                         return $AssAOM $Assignment (idd, op, ex)) <|> (do res "("; args <- sepBy expression comma; res ")"; semicolon; return $MethAOM $MethodCall (id,args))
                 Array id -> (do op <- assignOp; ex <- expression; semicolon;
                                 return $AssAOM $Assignment (idd, op, ex))


statement = foldr (<|>) (fail "error parsing statement") stparsers


assignOp = (res "=" >> return Eq) <|> (try $res "+=" >> return Pleq) <|> (try $res "-=" >> return Mineq)

location = genvarid

exparsers = [ do i <- genvarid;
                 case i of 
                   Array _ -> return $LocExpr i;
                   Basic x -> (do res "("; args <- sepBy expression comma; 
                                  res ")"; return $CallExpr $MethodCall (x,args)) <|> (return $LocExpr i)
--            , literal >>= LiteralExp
            ]


--expprim exp = do exp 
--expression = foldr (<|>) (fail "error parsing expression") (exparsers 

--expression = genvarid >>= return.LocExp

expression = do
      e <- try expression'
      t <- many expressionTail
      return $ Expression' e t

expressionTail = do
            b <- binop
            e <- expression'
            return (b, e)

expression' = (terminalExpr >>= return . TermExpr)
              <|> (do 
                    res "("
                    e <- expression
                    res ")"
                    return $ ParenExpr e)

              <|> (do
                    res "!"
                    e <- expression'
                    return $ NotExpr e)
              <|> (do
                    res "-"
                    e <- expression'
                    return $ MinExpr e)
              <?> "expression'"

terminalExpr = 
    (try (do i <- genvarid;
             case i of 
               Array _ -> return $LocExpr i;
               Basic x -> (do res "("; args <- sepBy expression comma; 
                              res ")"; return $CallExpr $MethodCall (x,args)) <|> (return $LocExpr i))) <|> (literal >>= return . LitExpr) <?> "terminal expression"


--terminalExpr = literal >>= return . LitExpr

binop = (arithOp >>= return.ArithBin) 
        <|> (relOp >>= return.RelBin)
        <|> (eqOp >>= return.EqBin)
        <|> (condOp >>= return.CondBin)

arithOp = (res "+" >> return Plus) <|> (res "-" >> return Minus) <|> (res "*" >> return Times)
          <|> (res "/" >> return Divide) <|> (res "%" >> return Mod)
relOp = try (res "<=" >> return LessEq) <|> try (res ">=" >> return GreaterEq) 
        <|> (res "<" >> return Less) <|> (res ">" >> return Greater)
eqOp = try (res "==" >> return EqEq) <|> try (res "!=" >> return NotEq)
condOp = (res "&&" >> return And) <|> (res "||" >> return Or)

literal = (int >>= return.IntLiteral) <|> (Main.string >>= return.StringLiteral)
          <|> (bool >>= return.BoolLiteral) <|> (Main.char >>= return.CharLiteral)

dtype = (res "int" >> return DInteger) <|> (res "boolean" >> return DBoolean)
returntype = (dtype >>= return.PrimitiveType)  <|> (res "void" >> return Void)

genvarid = do i <- did
              (do (res "["); size <- int; (res "]"); 
                  return $Array (i,size)) <|> (return $Basic i)

methodName = did




main :: IO ()
main = do args <- getArgs 
          str <- readFile (head args)
          tokens <- return (beginScanString str)
          str <- case runParser program () "" tokens of
            Left err  -> exitWith ExitSuccess
            Right val -> exitWith $ExitFailure 22

          putStrLn str

