module Decaf.AST where
import Numeric
import Decaf.Data.Tree

-- | ASTNode encapsulates common operations on the Abstract Syntax Tree
-- @pos - retrieves the parsed source code position of the 'ASTNode' as a 'DecafPosition'
-- @pp - pretty print the 'ASTNode'
-- @treeify - convert the subtree rooted at the 'ASTNode' into a generic 'Tree'

class ASTNode a where
    pos :: a -> DecafPosition   -- return the source position
    pp :: a -> String           -- pretty print
    treeify :: a -> Tree String -- turn into a generic tree

-- | ASTNode encapsulates common operations on the Abstract Syntax Tree
class Location a where
    ident :: a -> DecafIdentifier

-- |Abstract Syntax: Top-level structure of a decaf program
data DecafProgram = DecafProgram
    { fields :: [DecafField]
    , methods :: [DecafMethod]
    , progPos :: DecafPosition
    } deriving (Show, Eq)

-- |Abstract Syntax: Field Declaration: variable or array
data DecafField = DecafVarField DecafVar DecafPosition
               | DecafArrField DecafArr DecafPosition
               deriving (Show, Eq)

-- |Abstract Syntax: Method Declaration (Signature)
data DecafMethod = DecafMethod
    { methodType :: DecafType
    , methodID :: DecafIdentifier
    , methodArg :: [DecafVar]
    , methodBody :: DecafBlock
    , methodPos :: DecafPosition
} deriving (Show, Eq)

-- |Abstract Syntax: Variable Declaration (Signature)
data DecafVar = DecafVar
    { varType :: DecafType
    , varID :: DecafIdentifier
    , varPos :: DecafPosition
    } deriving (Show, Eq)

-- |Abstract Syntax: Array Declaration (Signature)
data DecafArr = DecafArr
    { arrayType :: DecafType
    , arrayID :: DecafIdentifier
    , arrayLength :: DecafInteger
    ,arrayPos :: DecafPosition
    } deriving (Show, Eq)

-- |Abstract Syntax: Compound Statement/Block
data DecafBlock = DecafBlock
    { blockVars :: [DecafVar]
    , blockStms :: [DecafStm]
    , blockPos :: DecafPosition
    } deriving (Show, Eq)

-- |Abstract Syntax: Type Enumeration
-- types : consider differentiating the two kinds of types
-- (one is an expression type (int|bool) and the other is method return type (int|bool|void))
data DecafType = DecafInteger
               | DecafBoolean
               | DecafVoid
               deriving (Show, Eq)

-- |Abstract Syntax: Statement
data DecafStm = DecafAssignStm DecafLoc DecafAssignOp DecafExpr DecafPosition
              | DecafMethodStm DecafMethodCall DecafPosition
              | DecafIfStm DecafExpr DecafBlock (Maybe DecafBlock) DecafPosition
              | DecafForStm DecafIdentifier DecafExpr DecafExpr DecafBlock DecafPosition
              | DecafRetStm (Maybe DecafExpr) DecafPosition
              | DecafBreakStm DecafPosition
              | DecafContStm DecafPosition
              | DecafBlockStm DecafBlock DecafPosition
              deriving (Show, Eq)

-- |Abstract Syntax: Assignment Operator, used in 'DecafAssignStm'
data DecafAssignOp = DecafEq DecafPosition
                   | DecafPlusEq DecafPosition
                   | DecafMinusEq DecafPosition
                   deriving (Show, Eq)

-- |Abstract Syntax: Method call, either defined within Decaf or externally, used in 'DecafAssignStm'
data DecafMethodCall = DecafPureMethodCall { methodCallID :: DecafIdentifier, methodCallArgs :: [DecafExpr], methodCallPos :: DecafPosition}
                     | DecafMethodCallout { methodCalloutID :: DecafString, methodCalloutArgs :: [DecafCalloutArg], methodCalloutPos :: DecafPosition}
                     deriving (Show, Eq)

-- |Abstract Syntax: Argument passed to 'DecafMethodCallout'
data DecafCalloutArg = DecafCalloutArgExpr DecafExpr DecafPosition
                     | DecafCalloutArgStr DecafString DecafPosition
                     deriving (Show, Eq)

-- |Abstract Syntax: Memory Location
data DecafLoc = DecafVarLoc DecafIdentifier DecafPosition
              | DecafArrLoc DecafIdentifier DecafExpr DecafPosition
              deriving (Show, Eq)

-- |Abstract Syntax: An abstract Decaf Expression Tree.
-- 'DecafExpr' is a temporory tree utilized for parsing in a left-associative, right recursive manner.
--  This temporary tree is rewritten to utilize the pure abstract DecafExpr tree after parsing.
--  For more information, see the 'rewriteExpr' function in 'Decaf.Parse'
data DecafExpr = DecafExpr Term Expr' DecafPosition -- used for parsing, but removed at tree rewrite
               | DecafLocExpr DecafLoc DecafPosition
               | DecafMethodExpr DecafMethodCall DecafPosition
               | DecafLitExpr DecafLiteral DecafPosition
               | DecafBinExpr DecafExpr DecafBinOp DecafExpr DecafPosition
               | DecafNotExpr DecafExpr DecafPosition
               | DecafMinExpr DecafExpr DecafPosition
               | DecafParenExpr DecafExpr DecafPosition
               deriving (Show, Eq)

data Expr' = Expr' DecafBinOp Term Expr' DecafPosition
           | EmptyExpr'
           deriving (Show, Eq)

data Term = Term Factor Term' DecafPosition
          deriving (Show, Eq)

data Term' = Term' DecafBinOp Factor Term' DecafPosition
           | EmptyTerm'
           deriving (Show, Eq)

data Factor = DecafParenExpr' DecafExpr DecafPosition
            | DecafNotExpr' DecafExpr DecafPosition
            | DecafMinExpr' DecafExpr DecafPosition
            | DecafLocExpr' DecafLoc DecafPosition
            | DecafMethodExpr' DecafMethodCall DecafPosition
            | DecafLitExpr' DecafLiteral DecafPosition
            deriving (Show, Eq)

-- |Abstract Syntax: A binary operation; used in 'DecafExpr' trees.
data DecafBinOp = DecafBinArithOp DecafArithOp DecafPosition
                | DecafBinRelOp DecafRelOp DecafPosition
                | DecafBinEqOp DecafEqOp DecafPosition
                | DecafBinCondOp DecafCondOp DecafPosition
                deriving (Show, Eq)

-- |Abstract Syntax: A binary arithmetic operation; of type 'DecafBinOp'
data DecafArithOp = DecafPlusOp DecafPosition
                  | DecafMinOp DecafPosition
                  | DecafMulOp DecafPosition
                  | DecafDivOp DecafPosition
                  | DecafModOp DecafPosition
                  deriving (Show, Eq)

-- |Abstract Syntax: A binary ralitonal comparison operation; of type 'DecafBinOp'
data DecafRelOp = DecafLTOp DecafPosition
                | DecafGTOp DecafPosition
                | DecafLTEOp DecafPosition
                | DecafGTEOp DecafPosition
                deriving (Show, Eq)

-- |Abstract Syntax: A binary equality comparison operation; of type 'DecafBinOp'
data DecafEqOp = DecafEqOp DecafPosition
               | DecafNEqOp DecafPosition
               deriving (Show, Eq)

-- |Abstract Syntax: A binary boolean conditional comparison operation; of type 'DecafBinOp'
data DecafCondOp = DecafAndOp DecafPosition
                 | DecafOrOp DecafPosition
                 deriving (Show, Eq)

-- |Abstract Syntax: A literal construct.
data DecafLiteral = DecafIntLit DecafInteger DecafPosition
               | DecafBoolLit Bool  DecafPosition
               | DecafStrLit  DecafString DecafPosition
               | DecafCharLit  DecafCharacter DecafPosition
               deriving (Show, Eq)

-- |Abstract Syntax: Integers can be either base Decimal or Hexadecimal.
data DecafInteger = DecafDec String
                  | DecafHex  String
                  deriving (Show, Eq)

-- |Abstract Syntax: We represent DecafString with a Haskell String.
type DecafString = String

-- |Abstract Syntax: We represent DecafBoolean with a Haskell Bool.
type DecafBoolean = Bool

-- |Abstract Syntax: We represent DecafCharacter with a Haskell Char.
type DecafCharacter = Char

-- |Abstract Syntax: An identifier in Decaf is represented with a Haskell String.
type DecafIdentifier = String

-- |Abstract Syntax: Every node in the Abstract Syntax tree track's it's parsed source pos as a tuple of (line, column)
type DecafPosition = (Int, Int)

instance ASTNode DecafProgram where
  pos (DecafProgram _ _ p) = p
  pp p = "Decaf Program" ++ "\n" ++ concatMap pp (fields p) ++ "\n" ++ concatMap pp (methods p)
  treeify p  = Node "Program" children
                    where
                      fs = map treeify (fields p)
                      ms = map treeify (methods p)
                      children = fs ++ ms


instance ASTNode DecafField where
  pos (DecafVarField _ p) = p
  pos (DecafArrField _ p) = p
  pp (DecafVarField v _) = "DecafVarField " ++ pp v ++ "\n"
  pp (DecafArrField a _) = "DecafArrField " ++ pp a ++ "\n"
  treeify (DecafVarField var _) = treeify var
  treeify (DecafArrField (DecafArr ty identf len _) _) = Node ("ARR " ++ pp ty ++ " " ++ identf ++"["++ pp len ++ "]") []

instance ASTNode DecafMethod where
  pos (DecafMethod _ _ _ _ p) = p
  pp (DecafMethod ty identf args _ _) = "METHOD " ++ pp ty ++ " " ++ identf ++ " " ++ unwords (map pp args)
  treeify meth@(DecafMethod _ _ _ body _) = Node (pp meth) [treeify body]

instance Location DecafMethod where
  ident (DecafMethod _ identf _ _ _) = identf

instance ASTNode DecafVar where
  pos (DecafVar _ _ p) = p
  pp (DecafVar ty identf _) = "VAR "++ pp ty++ " " ++ identf
  treeify var = Node (pp var) []

instance Location DecafVar where
  ident (DecafVar _ identf _) = identf

instance ASTNode DecafArr where
  pos (DecafArr _ _ _ p) = p
  pp (DecafArr ty identf len _) = "ARR " ++ pp ty ++ " " ++ identf ++"["++ pp len ++ "]"
  treeify a = Node (pp a) []

instance Location DecafArr where
  ident (DecafArr _ identf _ _) = identf

instance ASTNode DecafBlock where
  pos (DecafBlock _ _ p) = p
  treeify (DecafBlock vars stms _) = Node "Block" (vs ++ ss)
                                   where
                                     vs = map treeify vars
                                     ss = map treeify stms
  pp _ = "DecafBlock"

instance ASTNode DecafType where
  pp (DecafInteger) = "int"
  pp (DecafVoid)    = "void"
  pp (DecafBoolean) = "bool"
  pos _ = error "DecafType has no associated position"
  treeify a = Node (pp a) []

instance ASTNode DecafStm where
  pos (DecafAssignStm _ _ _ p) = p
  pos (DecafMethodStm _ p) = p
  pos (DecafIfStm _ _ _ p) = p
  pos (DecafForStm _ _ _ _ p) = p
  pos (DecafRetStm _ p) = p
  pos (DecafBreakStm p) = p
  pos (DecafContStm p) = p
  pos (DecafBlockStm _ p) = p
  treeify (DecafAssignStm loc op expr _) = Node (pp op) [treeify loc, treeify expr]
  treeify (DecafMethodStm m _) = treeify m
  treeify (DecafIfStm expr block elseblock _) = Node "if" (treeify expr : treeify block : elseblock')
                                              where
                                                elseblock' = case elseblock of
                                                                Just block -> [treeify block]
                                                                Nothing -> []
  
  treeify (DecafForStm identf expr expr' block _) = Node "for" ([Node identf []] ++ [treeify expr] ++ [treeify expr'] ++ [treeify block])
  treeify (DecafRetStm expr _) = Node "ret" (convertRetExpr expr)
    where
      convertRetExpr :: Maybe DecafExpr -> [Tree String]
      convertRetExpr (Just expr) = [treeify expr]
      convertRetExpr Nothing = []
  treeify (DecafBreakStm _) = Node "brk" []
  treeify (DecafContStm _) = Node "cnt" []
  treeify (DecafBlockStm block _) = Node "blockStm" [treeify block]
  pp _ = "DecafStm"

instance ASTNode DecafAssignOp where
  pos (DecafEq p) = p
  pos (DecafPlusEq p) = p 
  pos (DecafMinusEq p) = p
  pp (DecafEq _) = "="
  pp (DecafPlusEq _) = "+="
  pp (DecafMinusEq _) = "-="
  treeify a = Node (pp a) []
  
instance ASTNode DecafMethodCall where
  pos (DecafPureMethodCall _ _ p) = p
  pos (DecafMethodCallout _ _ p) = p
  pp (DecafPureMethodCall identf _ _) = "PureMethodcall[" ++ identf ++ "]"
  pp (DecafMethodCallout identf _ _) = "MethodCallOut[" ++ identf ++"]"
  treeify meth@(DecafPureMethodCall _ args _) = (Node $ pp meth) (map treeify args)
  treeify meth@(DecafMethodCallout _ args _) = (Node $ pp meth) (map treeify args)

instance ASTNode DecafLoc where
  pos (DecafVarLoc _ p) = p
  pos (DecafArrLoc _ _ p) = p
  pp (DecafVarLoc identf _) = identf
  pp (DecafArrLoc identf _ _) = identf ++ "[e]"
  treeify (DecafVarLoc identf _) = Node identf []
  treeify (DecafArrLoc identf expr _) = Node (identf ++ " []") [treeify expr]

instance Location DecafLoc where
  ident (DecafVarLoc identf _) = identf
  ident (DecafArrLoc identf _ _) = identf

instance ASTNode DecafCalloutArg where
  pos (DecafCalloutArgExpr _ p) = p
  pos (DecafCalloutArgStr _ p) = p
  treeify (DecafCalloutArgExpr expr _) = treeify expr
  treeify (DecafCalloutArgStr str _) = Node str []
  pp _ = "DecafCalloutArg"

instance ASTNode DecafExpr where
  pos (DecafExpr _ _ p) = p
  pos (DecafLocExpr _ p) = p
  pos (DecafMethodExpr _ p) = p
  pos (DecafLitExpr _ p) = p
  pos (DecafBinExpr _ _ _ p) = p
  pos (DecafNotExpr _ p) = p
  pos (DecafMinExpr _ p) = p
  pos (DecafParenExpr _ p) = p
  treeify (DecafLocExpr loc _) = treeify loc
  treeify (DecafMethodExpr meth _) = treeify meth
  treeify (DecafLitExpr lit _) = treeify lit
  treeify (DecafBinExpr expr binop expr' _) = Node (pp binop) [treeify expr, treeify expr']
  treeify (DecafNotExpr expr _) = Node "!" [treeify expr]
  treeify (DecafMinExpr expr _) = Node "-" [treeify expr]
  treeify (DecafParenExpr expr _) = Node "(   )" [treeify expr]
  treeify (DecafExpr term expr' _) = Node "EXPR" [treeify term, treeify expr']
  pp _ = "Expr"

instance ASTNode Expr' where
  pos (Expr' _ _ _ p) = p
  pos (EmptyExpr') = error "EmptyExpr' has no associated position"
  treeify (Expr' binop term expr' _) = Node (pp binop) [treeify term, treeify expr']
  treeify (EmptyExpr') = Node "EmptyExpr'" []
  pp _ = "Expr'"

instance ASTNode Term where
  pos (Term _ _ p) = p
  treeify (Term factor term' _) = Node "TERM" [treeify factor, treeify term']
  pp _ = "Term"

instance ASTNode Term' where
  pos (Term' _ _ _ p) = p
  pos (EmptyTerm') = error "EmptyTerm' node has no associated position"
  treeify (Term' binop factor term' _) = (Node $ pp binop) [treeify factor, treeify term']
  treeify (EmptyTerm') = Node "EmptyTerm'" []
  pp _ = "Term'"

instance ASTNode Factor where
  pos (DecafParenExpr' _ p) = p
  pos (DecafNotExpr' _ p) = p
  pos (DecafMinExpr' _ p) = p
  pos (DecafLocExpr' _ p) = p
  pos (DecafMethodExpr' _ p) = p
  pos (DecafLitExpr' _ p) = p
  treeify (DecafParenExpr' expr _) = Node "(   )" [treeify expr]
  treeify (DecafNotExpr' expr _) = Node "!" [treeify expr]
  treeify (DecafMinExpr' expr _) = Node "-" [treeify expr]
  treeify (DecafLocExpr' loc _) = treeify loc
  treeify (DecafMethodExpr' dmc _) = treeify dmc
  treeify (DecafLitExpr' dl _) = treeify dl
  pp _ = "Factor"

instance ASTNode DecafBinOp where
  pos (DecafBinArithOp _ p) = p
  pos (DecafBinRelOp _ p) = p
  pos (DecafBinEqOp _ p) = p
  pos (DecafBinCondOp _ p) = p
  pp (DecafBinArithOp o _) = pp o
  pp (DecafBinRelOp o _) = pp o
  pp (DecafBinEqOp o _) = pp o
  pp (DecafBinCondOp o _) = pp o
  treeify a = Node (pp a) []

instance ASTNode DecafArithOp where
  pos (DecafPlusOp p) = p
  pos (DecafMinOp p)  = p
  pos (DecafMulOp p)  = p
  pos (DecafModOp p)  = p
  pos (DecafDivOp p)  = p
  pp (DecafPlusOp _) = "+"
  pp (DecafMinOp _)  = "-"
  pp (DecafMulOp _)  = "*"
  pp (DecafModOp _)  = "%"
  pp (DecafDivOp _)  = "/"
  treeify a = Node (pp a) []

instance ASTNode DecafRelOp where
  pos (DecafLTOp p)  = p
  pos (DecafGTOp p)  = p
  pos (DecafGTEOp p) = p
  pos (DecafLTEOp p) = p
  pp (DecafLTOp _)  = "<"
  pp (DecafGTOp _)  = ">"
  pp (DecafGTEOp _) = ">="
  pp (DecafLTEOp _) = "<="
  treeify a = Node (pp a) []
  
instance ASTNode DecafEqOp where
  pos (DecafEqOp p)   = p
  pos (DecafNEqOp p) = p
  pp (DecafEqOp _)   = "=="
  pp (DecafNEqOp _) = "!="
  treeify a = Node (pp a) []

instance ASTNode DecafCondOp where
  pos (DecafAndOp p) = p
  pos (DecafOrOp p)  = p
  pp (DecafAndOp _) = "&&"
  pp (DecafOrOp _)  = "||"
  treeify a = Node (pp a) []

instance ASTNode DecafLiteral where
  pos (DecafIntLit _ p) = p
  pos (DecafBoolLit _ p) = p
  pos (DecafStrLit _ p) = p
  pos (DecafCharLit _ p) = p
  pp (DecafIntLit i _) = show i
  pp (DecafBoolLit b _) = show b
  pp (DecafStrLit s _) = show s
  pp (DecafCharLit c _) = show c
  treeify (DecafIntLit i _) = Node (pp i) []
  treeify (DecafBoolLit b _) = Node (show b) []
  treeify (DecafStrLit s _) = Node s []
  treeify (DecafCharLit c _) = Node (show c) []
  
instance ASTNode DecafInteger where
  pp (DecafDec s) = "0d"++s
  pp (DecafHex h) = "0x"++h
  treeify _ = Node "DecafInteger" []
  pos _     = error "DecafInteger has no associated position"

-- | 'readDecafInteger' parses a DecafInteger ASTNode and returns a Haskell integer
readDecafInteger :: DecafInteger -> Integer
readDecafInteger (DecafDec s) =
    if head s == '-'
      then -(read (tail s) :: Integer)
      else read  s :: Integer

readDecafInteger (DecafHex s) =
    if head s == '-'
      then -(fst . head . Numeric.readHex . tail $ s)
      else fst . head . Numeric.readHex $ s
