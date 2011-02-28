module Decaf.AST
where
import Data.List

-- type classes
class ASTNode a where
  pp :: a -> String -- pretty print
  treeify :: a -> Tree String -- turn into a generic tree

class Location a where
  ident :: a -> DecafIdentifier

-- generic tree
data Tree a = Node a (Maybe [Tree a])
            | Nil
            deriving (Eq, Show)

-- abstract syntax
data DecafProgram = DecafProgram {
    fields :: [DecafField],
    methods :: [DecafMethod]
} deriving (Show, Eq)

instance ASTNode DecafProgram where
  pp p = "Decaf Program" ++ "\n" ++ concat (map pp $ fields p) ++ "\n" ++ concat (map pp $ methods p)
  treeify p  = Node ( "Program") (Just children)
                    where
                      fs = map treeify (fields p)
                      ms = map treeify (methods p)
                      children = fs ++ ms

-- field declaration :: Variable or Array
data DecafField = DecafVarField DecafVar
               | DecafArrField DecafArr
               deriving (Show, Eq)

instance ASTNode DecafField where
  pp (DecafVarField v) = "DecafVarField " ++ pp v ++ "\n"
  pp (DecafArrField a) = "DecafArrField " ++ pp a ++ "\n"
  treeify (DecafVarField var@(DecafVar ty identf)) = treeify var
  treeify (DecafArrField (DecafArr ty identf len)) = Node ("ARR " ++ pp ty ++ " " ++ identf ++"["++ pp len ++ "]") Nothing
-- method declaration
data DecafMethod = DecafMethod {
    methodType :: DecafType,
    methodID :: DecafIdentifier,
    methodArg :: [DecafVar],
    methodBody :: DecafBlock
} deriving (Show, Eq)

instance ASTNode DecafMethod where
  pp (DecafMethod ty identf args body) = "METHOD " ++ pp ty ++ " " ++ identf ++ " " ++ (concat $ intersperse " " $ map pp args)
  treeify meth@(DecafMethod ty identf args body) = (Node $ pp meth) (Just $ [treeify body])

instance Location DecafMethod where
  ident (DecafMethod ty identf args body) = identf

-- variable declaration
data DecafVar = DecafVar {
    varType :: DecafType,
    varID :: DecafIdentifier
} deriving (Show, Eq)

instance ASTNode DecafVar where
  pp (DecafVar ty identf) = "VAR "++ pp ty++ " " ++ identf
  treeify var@(DecafVar ty identf) = Node (pp var) Nothing

instance Location DecafVar where
  ident (DecafVar ty identf) = identf

-- array declaration
data DecafArr = DecafArr {
    arrayType :: DecafType,
    arrayID :: DecafIdentifier,
    arrayLength :: DecafInteger
} deriving (Show, Eq)

instance ASTNode DecafArr where
  pp (DecafArr ty identf len) = "ARR " ++ pp ty ++ " " ++ identf ++"["++ pp len ++ "]"

instance Location DecafArr where
  ident (DecafArr ty identf len) = identf

-- block body
data DecafBlock = DecafBlock {
    blockVars :: [DecafVar],
    blockStms :: [DecafStm]
} deriving (Show, Eq)

instance ASTNode DecafBlock where
  treeify (DecafBlock vars stms) = (Node "Block") (Just $ vs ++ ss)
                                   where
                                     vs = map treeify vars
                                     ss = map treeify stms

-- types : consider differentiating the two kinds of types
-- (one is an expression type (int|bool) and the other is method return type (int|bool|void))
data DecafType = DecafInteger
               | DecafBoolean
               | DecafVoid
               deriving (Show, Eq)

instance ASTNode DecafType where
  pp (DecafInteger) = "int"
  pp (DecafVoid)    = "void"
  pp (DecafBoolean) = "bool"

data DecafStm = DecafAssignStm DecafLoc DecafAssignOp DecafExpr
              | DecafMethodStm DecafMethodCall
              | DecafIfStm DecafExpr DecafBlock (Maybe DecafBlock)
              | DecafForStm DecafIdentifier DecafExpr DecafExpr DecafBlock
              | DecafRetStm (Maybe DecafExpr)
              | DecafBreakStm
              | DecafContStm
              | DecafBlockStm DecafBlock
              deriving (Show, Eq)

instance ASTNode DecafStm where
  treeify (DecafAssignStm loc op expr) = (Node $ pp op) (Just $ [treeify loc] ++ [treeify expr])
  treeify (DecafMethodStm m@(DecafPureMethodCall identf args)) = treeify m
  treeify (DecafMethodStm m@(DecafMethodCallout identf args)) = treeify m
  treeify (DecafIfStm expr block elseblock) = Node ( "if") (Just $ [treeify expr] ++ [treeify block] ++ elseblock')
                                              where
                                                elseblock' = case elseblock of
                                                                Just block -> [treeify block]
                                                                Nothing -> [Nil]
  
  treeify (DecafForStm identf expr expr' block) = Node ( "for") (Just $ [(Node identf) Nothing] ++ [treeify expr] ++ [treeify expr'] ++ [treeify block])
  treeify (DecafRetStm expr) = Node ( "ret") $ convertRetExpr expr
    where
      convertRetExpr :: (Maybe DecafExpr) -> (Maybe [Tree String])
      convertRetExpr (Just expr) = Just $ [treeify expr]
      convertRetExpr Nothing = Nothing
  treeify (DecafBreakStm) = Node ( "brk") Nothing
  treeify (DecafContStm) = Node ( "cnt") Nothing
  treeify (DecafBlockStm block) = Node ("blockStm") (Just $ [treeify block])
      

data DecafAssignOp = DecafEq
                   | DecafPlusEq
                   | DecafMinusEq
                   deriving (Show, Eq)

instance ASTNode DecafAssignOp where
  pp (DecafEq) = "="
  pp (DecafPlusEq) = "+="
  pp (DecafMinusEq) = "-="
  
data DecafMethodCall = DecafPureMethodCall { methodCallID :: DecafIdentifier, methodCallArgs :: [DecafExpr] }
                     | DecafMethodCallout { methodCalloutID :: DecafString, methodCalloutArgs :: [DecafCalloutArg] }
                     deriving (Show, Eq)

instance ASTNode DecafMethodCall where
  pp (DecafPureMethodCall identf args) = "PureMethodcall[" ++ identf ++ "]"
  pp (DecafMethodCallout identf args) = "MethodCallOut[" ++ identf ++"]"
  treeify meth@(DecafPureMethodCall identf args) = (Node $ pp meth) (Just $ map treeify args)
  treeify meth@(DecafMethodCallout identf args) = (Node $ pp meth) (Just $ map treeify args)

data DecafLoc = DecafVarLoc DecafIdentifier
              | DecafArrLoc DecafIdentifier DecafExpr
              deriving (Show, Eq)

instance ASTNode DecafLoc where
  pp (DecafVarLoc identf) = identf
  pp (DecafArrLoc identf expr) = identf ++ "[e]"
  treeify (DecafVarLoc identf) = (Node identf) Nothing
  treeify (DecafArrLoc identf expr) = Node (identf ++ " []") (Just $ [treeify expr])

data DecafCalloutArg = DecafCalloutArgExpr DecafExpr
                     | DecafCalloutArgStr DecafString
                     deriving (Show, Eq)
instance ASTNode DecafCalloutArg where
  treeify (DecafCalloutArgExpr expr) = treeify expr
  treeify (DecafCalloutArgStr str) = (Node str) Nothing

data DecafExpr = DecafExpr Term Expr' -- used for parsing, but removed at tree rewrite
               | DecafLocExpr DecafLoc
               | DecafMethodExpr DecafMethodCall
               | DecafLitExpr DecafLiteral
               | DecafBinExpr DecafBinOp DecafExpr
               | DecafNotExpr DecafExpr
               | DecafMinExpr DecafExpr
               | DecafParenExpr DecafExpr
               deriving (Show, Eq)

instance ASTNode DecafExpr where
  treeify (DecafExpr term expr') = (Node "EXPR") (Just $ [treeify term] ++ [treeify expr'])

data Expr' = Expr' DecafBinOp Term Expr'
           | EmptyExpr'
           deriving (Show, Eq)

instance ASTNode Expr' where
  treeify (Expr' binop term expr') = (Node $ pp binop) (Just $ [treeify term] ++ [treeify expr'])
  treeify (EmptyExpr') = Nil

data Term = Term Factor Term'
          deriving (Show, Eq)

instance ASTNode Term where
  treeify (Term factor term') = (Node "TERM") (Just $ [treeify factor] ++ [treeify term'])

data Term' = Term' DecafBinOp Factor Term'
           | EmptyTerm'
           deriving (Show, Eq)

instance ASTNode Term' where
  treeify (Term' binop factor term') = (Node $ pp binop) (Just $ [treeify factor] ++ [treeify term'])
  treeify (EmptyTerm') = Nil

data Factor = DecafParenExpr' DecafExpr
            | DecafNotExpr' DecafExpr
            | DecafMinExpr' DecafExpr
            | DecafLocExpr' DecafLoc
            | DecafMethodExpr' DecafMethodCall
            | DecafLitExpr' DecafLiteral
            deriving (Show, Eq)

instance ASTNode Factor where
  treeify (DecafParenExpr' expr) = (Node "(   )") (Just [treeify expr])
  treeify (DecafNotExpr' expr) = (Node "!") (Just [treeify expr])
  treeify (DecafMinExpr' expr) = (Node "-") (Just [treeify expr])
  treeify (DecafLocExpr' loc) = treeify loc
  treeify (DecafMethodExpr' dmc) = treeify dmc
  treeify (DecafLitExpr' dl) = treeify dl

data DecafBinOp = DecafBinArithOp DecafArithOp
                | DecafBinRelOp DecafRelOp
                | DecafBinEqOp DecafEqOp
                | DecafBinCondOp DecafCondOp
                deriving (Show, Eq)

instance ASTNode DecafBinOp where
  pp (DecafBinArithOp o) = pp o
  pp (DecafBinRelOp o) = pp o
  pp (DecafBinEqOp o) = pp o
  pp (DecafBinCondOp o) = pp o

data DecafArithOp = DecafPlusOp
                  | DecafMinOp
                  | DecafMulOp
                  | DecafDivOp
                  | DecafModOp
                  deriving (Show, Eq)

instance ASTNode DecafArithOp where
  pp (DecafPlusOp) = "+"
  pp (DecafMinOp)  = "-"
  pp (DecafMulOp)  = "*"
  pp (DecafModOp)  = "%"
  pp (DecafDivOp)  = "/"

data DecafRelOp = DecafLTOp
                | DecafGTOp
                | DecafLTEOp
                | DecafGTEOp
                deriving (Show, Eq)

instance ASTNode DecafRelOp where
  pp (DecafLTOp)  = "<"
  pp (DecafGTOp)  = ">"
  pp (DecafGTEOp) = ">="
  pp (DecafLTEOp) = "<="
  
data DecafEqOp = DecafEqOp
               | DecafNEqOp
               deriving (Show, Eq)

instance ASTNode DecafEqOp where
  pp (DecafEqOp)   = "=="
  pp (DecafNEqOp) = "!="

data DecafCondOp = DecafAndOp
                 | DecafOrOp
                 deriving (Show, Eq)

instance ASTNode DecafCondOp where
  pp (DecafAndOp) = "&&"
  pp (DecafOrOp)  = "||"

data DecafLiteral = DecafIntLit DecafInteger
               | DecafBoolLit Bool 
               | DecafStrLit  DecafString
               | DecafCharLit  DecafCharacter
               deriving (Show, Eq)

instance ASTNode DecafLiteral where
  pp (DecafIntLit i) = show i
  pp (DecafBoolLit b) = show b
  pp (DecafStrLit s) = show s
  pp (DecafCharLit c) = show c
  treeify (DecafIntLit i) = Node (pp i) Nothing
  treeify (DecafBoolLit b) = Node (show b) Nothing
  treeify (DecafStrLit s) = (Node s) Nothing
  treeify (DecafCharLit c) = (Node $ show c) Nothing
  
type DecafString = String

data DecafInteger = DecafDec String
                  | DecafHex  String
                  deriving (Show, Eq)

instance ASTNode DecafInteger where
  pp (DecafDec s) = "0d"++s
  pp (DecafHex h) = "0x"++h
  
type DecafBoolean = Bool
type DecafCharacter = Char
type DecafIdentifier = String
