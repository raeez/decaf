module Decaf.Tokens
where
import Text.ParserCombinators.Parsec

-- | The 'Position' type encapsulates the start and end position of a token in the source text
type Position = (SourcePos, SourcePos)

-- |The 'Token' type encapsulates a scanned token and it's corresponding Position in the source text
type Token = (Position, DecafToken)

-- |The 'showToken' function pretty prints a Token as a singel line
showToken :: (Position, DecafToken) -> String
showToken ((p1, p2), t) = "[L" ++ (show . sourceLine $ p1) ++ ":C" ++ (show . sourceColumn $ p1) ++ "-" ++ (show . sourceColumn $ p2) ++ "] " ++ (show t)

-- |The 'getStart' function retrieves the start position of a Token as a Parsec SourcePos
getStart :: (Position, DecafToken) -> SourcePos
getStart ((p1, p2), _) = p1

-- |The 'getEnd' function retrieves the end position of a Token as a Parsec SourcePos
getEnd :: (Position, DecafToken) -> SourcePos
getEnd ((p1, p2), _) = p2

-- |The 'getToken' function retrieves the DecafToken from a 'Token' type
getToken :: (Position, DecafToken) -> DecafToken
getToken (_, t) = t

-- |The 'DecafToken' ADT represents a single scanned Token in the Decaf language 
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

instance Show DecafToken where
  show (StrLit s) = "STRINGLITERAL \"" ++ strShow s ++ "\""
    where
      strShow [] = []
      strShow (c:cs)
            | c == '\t' = ('\\' : 't' : strShow cs)
            | c == '\n' = ('\\' : 'n' : strShow cs)
            | c == '\"' = ('\\' : '"' : strShow cs)
            | c == '\'' = ('\\' : '\'' : strShow cs)
            | c == '\\' = ('\\' : '\\' : strShow cs)
            | otherwise = (c:strShow cs)

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

-- | Report utilized throughout the binaries for error reporting
data Report a = RSuccess a
              | RError String

getSuccess (RSuccess a) = a
getFailure (RError s) = s

getReport :: Report a -> String
getReport (RSuccess a) = show a
getReport (RError s) = s
