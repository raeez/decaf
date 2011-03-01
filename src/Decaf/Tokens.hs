module Decaf.Tokens
where
import Text.ParserCombinators.Parsec

type Token = (SourcePos, SourcePos, DecafToken)

data DecafToken = Fail String
                | Identf String
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
                | LBrack
                | RBrack
                | Comma
                | Semi
                | OpAnd
                | OpOr
                | OpEq
                | OpNEq
                | OpLT
                | OpGT
                | OpLTE
                | OpGTE
                | OpAdd
                | OpMin
                | OpMul
                | OpDiv
                | OpMod
                | Assign
                | PlusAssign
                | MinusAssign
                | OpNot
                | EOF
                deriving (Eq)
strShow (c:cs) | c == '\t' = ('\\' : 't' : strShow cs)
             | c == '\n' = ('\\' : 'n' : strShow cs)
             | c == '\"' = ('\\' : '"' : strShow cs)
             | c == '\'' = ('\\' : '\'' : strShow cs)
             | c == '\\' = ('\\' : '\\' : strShow cs)
             | otherwise = (c:strShow cs)
strShow [] = []

instance Show DecafToken where
  show (StrLit s) = "STRINGLITERAL \"" ++ strShow s ++ "\""
  show (CharLit s) | s == '"' = "CHARLITERAL '\\\"'"
                   | otherwise  = "CHARLITERAL " ++ show s
  show (DecLit s) = "INTLITERAL " ++ s
  show (HexLit s) = "INTLITERAL 0x" ++ s
  show (BoolLit s) | s == False = "BOOLEANLITERAL false"
                   | s == True = "BOOLEANLITERAL true"
  show (Identf s) = "IDENTIFIER " ++ s
  show (Reserv s) = s
  show (LParen) = "("
  show (RParen) = ")"
  show (RBrace) = "}"
  show (LBrace) = "{"
  show (LBrack) = "["
  show (RBrack) = "]"
  show (Semi) = ";"
  show (OpAnd) = "&&"
  show (OpOr) = "||"
  show (Assign) = "="
  show (PlusAssign) = "+="
  show (MinusAssign) = "-="
  show (Comma) = ","
  show (OpNot) = "!"
  show (OpEq) = "=="
  show (OpNEq) = "!="
  show (OpLT) = "<"
  show (OpGT) = ">"
  show (OpLTE) = "<="
  show (OpGTE) = ">="
  show (OpAdd) = "+"
  show (OpMin) = "-"
  show (OpMul) = "*"
  show (OpDiv) = "/"
  show (OpMod) = "%"
  show (Fail s) = "SCANNER ERROR: " ++ s
  show (EOF) = "EOF"

showToken :: (SourcePos, SourcePos, DecafToken) -> String
showToken (p1, p2, t) = "[L" ++ (show . sourceLine $ p1) ++ ":C" ++ (show . sourceColumn $ p1) ++ "-" ++ (show . sourceColumn $ p2) ++ "] " ++ (show t)

startPos :: (SourcePos, SourcePos, DecafToken) -> SourcePos
startPos (p1, p2, t) = p1

endPos (p1, p2, t) = p2

dToken :: (SourcePos, SourcePos, DecafToken) -> DecafToken
dToken (p1, p2, t) = t



-- Report  moved from Parser.hs -J
data Report a = RSuccess a
              | RError String
                deriving (Show,Eq)

{-
instance Show Report a where
  show (RSuccess a) = "Success" ++ show a
  show (RError str) = "Error" ++ str
  -}
                    
getSuccess (RSuccess a) = a

getReport (RSuccess a) = show a
getReport (RError s) = s