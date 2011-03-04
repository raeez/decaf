module Decaf.AST
where
import Data.List

-- | ASTNode encapsulates common operations on the Abstract Syntax Tree
-- @pos - retrieves the parsed source code position of the 'ASTNode' as a 'DecafPosition'
-- @pp - pretty print the 'ASTNode'
-- @treeify - convert the subtree rooted at the 'ASTNode' into a generic 'Tree'
class ASTNode a where
  pos :: a -> DecafPosition
  pp :: a -> String -- pretty print
  treeify :: a -> Tree String -- turn into a generic tree

-- | ASTNode encapsulates common operations on the Abstract Syntax Tree
class Location a where
  ident :: a -> DecafIdentifier

-- | A generic tree 
data Tree a = Node a (Maybe [Tree a])
            | Nil
            deriving (Eq, Show)

-- |Abstract Syntax: Top-level structure of a decaf program
data DecafProgram = DecafProgram {
    fields :: [DecafField],
    methods :: [DecafMethod],
    progPos :: DecafPosition
} deriving (Show, Eq)

-- |Abstract Syntax: Field Declaration: variable or array
data DecafField = DecafVarField DecafVar DecafPosition
               | DecafArrField DecafArr DecafPosition
               deriving (Show, Eq)

-- |Abstract Syntax: Method Declaration (Signature)
data DecafMethod = DecafMethod {
    methodType :: DecafType,
    methodID :: DecafIdentifier,
    methodArg :: [DecafVar],
    methodBody :: DecafBlock,
    methodPos :: DecafPosition
} deriving (Show, Eq)

-- |Abstract Syntax: Variable Declaration (Signature)
data DecafVar = DecafVar {
    varType :: DecafType,
    varID :: DecafIdentifier,
    varPos :: DecafPosition
} deriving (Show, Eq)

-- |Abstract Syntax: Array Declaration (Signature)
data DecafArr = DecafArr {
    arrayType :: DecafType,
    arrayID :: DecafIdentifier,
    arrayLength :: DecafInteger,
    arrayPos :: DecafPosition
} deriving (Show, Eq)

-- |Abstract Syntax: Compound Statement/Block
data DecafBlock = DecafBlock {
    blockVars :: [DecafVar],
    blockStms :: [DecafStm],
    blockPos :: DecafPosition
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
  pp p = "Decaf Program" ++ "\n" ++ concat (map pp $ fields p) ++ "\n" ++ concat (map pp $ methods p)
  treeify p  = Node ( "Program") (Just children)
                    where
                      fs = map treeify (fields p)
                      ms = map treeify (methods p)
                      children = fs ++ ms


instance ASTNode DecafField where
  pos (DecafVarField _ p) = p
  pos (DecafArrField _ p) = p
  pp (DecafVarField v _) = "DecafVarField " ++ pp v ++ "\n"
  pp (DecafArrField a _) = "DecafArrField " ++ pp a ++ "\n"
  treeify (DecafVarField var@(DecafVar ty identf _) _) = treeify var
  treeify (DecafArrField (DecafArr ty identf len _) _) = Node ("ARR " ++ pp ty ++ " " ++ identf ++"["++ pp len ++ "]") Nothing

instance ASTNode DecafMethod where
  pos (DecafMethod _ _ _ _ p) = p
  pp (DecafMethod ty identf args body _) = "METHOD " ++ pp ty ++ " " ++ identf ++ " " ++ (concat $ intersperse " " $ map pp args)
  treeify meth@(DecafMethod ty identf args body _) = (Node $ pp meth) (Just $ [treeify body])

instance Location DecafMethod where
  ident (DecafMethod ty identf args body _) = identf

instance ASTNode DecafVar where
  pos (DecafVar _ _ p) = p
  pp (DecafVar ty identf _) = "VAR "++ pp ty++ " " ++ identf
  treeify var@(DecafVar ty identf _) = Node (pp var) Nothing

instance Location DecafVar where
  ident (DecafVar ty identf _) = identf
instance ASTNode DecafArr where
  pos (DecafArr _ _ _ p) = p
  pp (DecafArr ty identf len _) = "ARR " ++ pp ty ++ " " ++ identf ++"["++ pp len ++ "]"

instance Location DecafArr where
  ident (DecafArr ty identf len _) = identf

instance ASTNode DecafBlock where
  pos (DecafBlock _ _ p) = p
  treeify (DecafBlock vars stms _) = (Node "Block") (Just $ vs ++ ss)
                                   where
                                     vs = map treeify vars
                                     ss = map treeify stms


instance ASTNode DecafType where
  pp (DecafInteger) = "int"
  pp (DecafVoid)    = "void"
  pp (DecafBoolean) = "bool"

instance ASTNode DecafStm where
  pos (DecafAssignStm _ _ _ p) = p
  pos (DecafMethodStm _ p) = p
  pos (DecafIfStm _ _ _ p) = p
  pos (DecafForStm _ _ _ _ p) = p
  pos (DecafRetStm _ p) = p
  pos (DecafBreakStm p) = p
  pos (DecafContStm p) = p
  pos (DecafBlockStm _ p) = p
  treeify (DecafAssignStm loc op expr _) = (Node $ pp op) (Just $ [treeify loc] ++ [treeify expr])
  treeify (DecafMethodStm m@(DecafPureMethodCall identf args _) _) = treeify m
  treeify (DecafMethodStm m@(DecafMethodCallout identf args _) _) = treeify m
  treeify (DecafIfStm expr block elseblock _) = Node ( "if") (Just $ [treeify expr] ++ [treeify block] ++ elseblock')
                                              where
                                                elseblock' = case elseblock of
                                                                Just block -> [treeify block]
                                                                Nothing -> [Nil]
  
  treeify (DecafForStm identf expr expr' block _) = Node ( "for") (Just $ [(Node identf) Nothing] ++ [treeify expr] ++ [treeify expr'] ++ [treeify block])
  treeify (DecafRetStm expr _) = Node ( "ret") $ convertRetExpr expr
    where
      convertRetExpr :: (Maybe DecafExpr) -> (Maybe [Tree String])
      convertRetExpr (Just expr) = Just $ [treeify expr]
      convertRetExpr Nothing = Nothing
  treeify (DecafBreakStm _) = Node ( "brk") Nothing
  treeify (DecafContStm _) = Node ( "cnt") Nothing
  treeify (DecafBlockStm block _) = Node ("blockStm") (Just $ [treeify block])

instance ASTNode DecafAssignOp where
  pos (DecafEq p) = p
  pos (DecafPlusEq p) = p 
  pos (DecafMinusEq p) = p
  pp (DecafEq _) = "="
  pp (DecafPlusEq _) = "+="
  pp (DecafMinusEq _) = "-="
  
instance ASTNode DecafMethodCall where
  pos (DecafPureMethodCall _ _ p) = p
  pos (DecafMethodCallout _ _ p) = p
  pp (DecafPureMethodCall identf args _) = "PureMethodcall[" ++ identf ++ "]"
  pp (DecafMethodCallout identf args _) = "MethodCallOut[" ++ identf ++"]"
  treeify meth@(DecafPureMethodCall identf args _) = (Node $ pp meth) (Just $ map treeify args)
  treeify meth@(DecafMethodCallout identf args _) = (Node $ pp meth) (Just $ map treeify args)

instance ASTNode DecafLoc where
  pos (DecafVarLoc _ p) = p
  pos (DecafArrLoc _ _ p) = p
  pp (DecafVarLoc identf _) = identf
  pp (DecafArrLoc identf expr _) = identf ++ "[e]"
  treeify (DecafVarLoc identf _) = (Node identf) Nothing
  treeify (DecafArrLoc identf expr _) = Node (identf ++ " []") (Just $ [treeify expr])

instance Location DecafLoc where
  ident (DecafVarLoc identf _) = identf
  ident (DecafArrLoc identf expr _) = identf


instance ASTNode DecafCalloutArg where
  pos (DecafCalloutArgExpr _ p) = p
  pos (DecafCalloutArgStr _ p) = p
  treeify (DecafCalloutArgExpr expr _) = treeify expr
  treeify (DecafCalloutArgStr str _) = (Node str) Nothing

instance ASTNode DecafExpr where
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
  treeify (DecafBinExpr expr binop expr' _) = (Node $ pp binop) (Just $ [treeify expr] ++ [treeify expr'])
  treeify (DecafNotExpr expr _) = (Node "!") (Just $ [treeify expr])
  treeify (DecafMinExpr expr _) = (Node "-") (Just $ [treeify expr])
  treeify (DecafParenExpr expr _) = (Node "(   )") (Just $ [treeify expr])
  treeify (DecafExpr term expr' _) = (Node "EXPR") (Just $ [treeify term] ++ [treeify expr'])

instance ASTNode Expr' where
  pos (Expr' _ _ _ p) = p
  treeify (Expr' binop term expr' _) = (Node $ pp binop) (Just $ [treeify term] ++ [treeify expr'])
  treeify (EmptyExpr') = Nil

instance ASTNode Term where
  pos (Term _ _ p) = p
  treeify (Term factor term' _) = (Node "TERM") (Just $ [treeify factor] ++ [treeify term'])

instance ASTNode Term' where
  pos (Term' _ _ _ p) = p
  treeify (Term' binop factor term' _) = (Node $ pp binop) (Just $ [treeify factor] ++ [treeify term'])
  treeify (EmptyTerm') = Nil

instance ASTNode Factor where
  pos (DecafParenExpr' _ p) = p
  pos (DecafNotExpr' _ p) = p
  pos (DecafMinExpr' _ p) = p
  pos (DecafLocExpr' _ p) = p
  pos (DecafMethodExpr' _ p) = p
  pos (DecafLitExpr' _ p) = p
  treeify (DecafParenExpr' expr _) = (Node "(   )") (Just [treeify expr])
  treeify (DecafNotExpr' expr _) = (Node "!") (Just [treeify expr])
  treeify (DecafMinExpr' expr _) = (Node "-") (Just [treeify expr])
  treeify (DecafLocExpr' loc _) = treeify loc
  treeify (DecafMethodExpr' dmc _) = treeify dmc
  treeify (DecafLitExpr' dl _) = treeify dl

instance ASTNode DecafBinOp where
  pos (DecafBinArithOp _ p) = p
  pos (DecafBinRelOp _ p) = p
  pos (DecafBinEqOp _ p) = p
  pos (DecafBinCondOp _ p) = p
  pp (DecafBinArithOp o _) = pp o
  pp (DecafBinRelOp o _) = pp o
  pp (DecafBinEqOp o _) = pp o
  pp (DecafBinCondOp o _) = pp o

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

instance ASTNode DecafRelOp where
  pos (DecafLTOp p)  = p
  pos (DecafGTOp p)  = p
  pos (DecafGTEOp p) = p
  pos (DecafLTEOp p) = p
  pp (DecafLTOp _)  = "<"
  pp (DecafGTOp _)  = ">"
  pp (DecafGTEOp _) = ">="
  pp (DecafLTEOp _) = "<="
  
instance ASTNode DecafEqOp where
  pos (DecafEqOp p)   = p
  pos (DecafNEqOp p) = p
  pp (DecafEqOp _)   = "=="
  pp (DecafNEqOp _) = "!="

instance ASTNode DecafCondOp where
  pos (DecafAndOp p) = p
  pos (DecafOrOp p)  = p
  pp (DecafAndOp _) = "&&"
  pp (DecafOrOp _)  = "||"

instance ASTNode DecafLiteral where
  pos (DecafIntLit _ p) = p
  pos (DecafBoolLit _ p) = p
  pos (DecafStrLit _ p) = p
  pos (DecafCharLit _ p) = p
  pp (DecafIntLit i _) = show i
  pp (DecafBoolLit b _) = show b
  pp (DecafStrLit s _) = show s
  pp (DecafCharLit c _) = show c
  treeify (DecafIntLit i _) = Node (pp i) Nothing
  treeify (DecafBoolLit b _) = Node (show b) Nothing
  treeify (DecafStrLit s _) = (Node s) Nothing
  treeify (DecafCharLit c _) = (Node $ show c) Nothing
  
instance ASTNode DecafInteger where
  pp (DecafDec s) = "0d"++s
  pp (DecafHex h) = "0x"++h
  
