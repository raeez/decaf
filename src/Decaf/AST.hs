module Decaf.AST
where
import Data.List

-- type classes
class ASTNode a where
  pos :: a -> DecafPosition
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
    methods :: [DecafMethod],
    progPos :: DecafPosition
} deriving (Show, Eq)

instance ASTNode DecafProgram where
  pos (DecafProgram _ _ p) = p
  pp p = "Decaf Program" ++ "\n" ++ concat (map pp $ fields p) ++ "\n" ++ concat (map pp $ methods p)
  treeify p  = Node ( "Program") (Just children)
                    where
                      fs = map treeify (fields p)
                      ms = map treeify (methods p)
                      children = fs ++ ms

-- field declaration :: Variable or Array
data DecafField = DecafVarField DecafVar DecafPosition
               | DecafArrField DecafArr DecafPosition
               deriving (Show, Eq)

instance ASTNode DecafField where
  pos (DecafVarField _ p) = p
  pos (DecafArrField _ p) = p
  pp (DecafVarField v _) = "DecafVarField " ++ pp v ++ "\n"
  pp (DecafArrField a _) = "DecafArrField " ++ pp a ++ "\n"
  treeify (DecafVarField var@(DecafVar ty identf _) _) = treeify var
  treeify (DecafArrField (DecafArr ty identf len _) _) = Node ("ARR " ++ pp ty ++ " " ++ identf ++"["++ pp len ++ "]") Nothing
-- method declaration
data DecafMethod = DecafMethod {
    methodType :: DecafType,
    methodID :: DecafIdentifier,
    methodArg :: [DecafVar],
    methodBody :: DecafBlock,
    methodPos :: DecafPosition
} deriving (Show, Eq)

instance ASTNode DecafMethod where
  pos (DecafMethod _ _ _ _ p) = p
  pp (DecafMethod ty identf args body _) = "METHOD " ++ pp ty ++ " " ++ identf ++ " " ++ (concat $ intersperse " " $ map pp args)
  treeify meth@(DecafMethod ty identf args body _) = (Node $ pp meth) (Just $ [treeify body])

instance Location DecafMethod where
  ident (DecafMethod ty identf args body _) = identf

-- variable declaration
data DecafVar = DecafVar {
    varType :: DecafType,
    varID :: DecafIdentifier,
    varPos :: DecafPosition
} deriving (Show, Eq)

instance ASTNode DecafVar where
  pos (DecafVar _ _ p) = p
  pp (DecafVar ty identf _) = "VAR "++ pp ty++ " " ++ identf
  treeify var@(DecafVar ty identf _) = Node (pp var) Nothing

instance Location DecafVar where
  ident (DecafVar ty identf _) = identf

-- array declaration
data DecafArr = DecafArr {
    arrayType :: DecafType,
    arrayID :: DecafIdentifier,
    arrayLength :: DecafInteger,
    arrayPos :: DecafPosition
} deriving (Show, Eq)

instance ASTNode DecafArr where
  pos (DecafArr _ _ _ p) = p
  pp (DecafArr ty identf len _) = "ARR " ++ pp ty ++ " " ++ identf ++"["++ pp len ++ "]"

instance Location DecafArr where
  ident (DecafArr ty identf len _) = identf

-- block body
data DecafBlock = DecafBlock {
    blockVars :: [DecafVar],
    blockStms :: [DecafStm],
    blockPos :: DecafPosition
} deriving (Show, Eq)

instance ASTNode DecafBlock where
  pos (DecafBlock _ _ p) = p
  treeify (DecafBlock vars stms _) = (Node "Block") (Just $ vs ++ ss)
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

data DecafStm = DecafAssignStm DecafLoc DecafAssignOp DecafExpr DecafPosition
              | DecafMethodStm DecafMethodCall DecafPosition
              | DecafIfStm DecafExpr DecafBlock (Maybe DecafBlock) DecafPosition
              | DecafForStm DecafIdentifier DecafExpr DecafExpr DecafBlock DecafPosition
              | DecafRetStm (Maybe DecafExpr) DecafPosition
              | DecafBreakStm DecafPosition
              | DecafContStm DecafPosition
              | DecafBlockStm DecafBlock DecafPosition
              deriving (Show, Eq)

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

data DecafAssignOp = DecafEq DecafPosition
                   | DecafPlusEq DecafPosition
                   | DecafMinusEq DecafPosition
                   deriving (Show, Eq)

instance ASTNode DecafAssignOp where
  pos (DecafEq p) = p
  pos (DecafPlusEq p) = p 
  pos (DecafMinusEq p) = p
  pp (DecafEq _) = "="
  pp (DecafPlusEq _) = "+="
  pp (DecafMinusEq _) = "-="
  
data DecafMethodCall = DecafPureMethodCall { methodCallID :: DecafIdentifier, methodCallArgs :: [DecafExpr], methodCallPos :: DecafPosition}
                     | DecafMethodCallout { methodCalloutID :: DecafString, methodCalloutArgs :: [DecafCalloutArg], methodCalloutPos :: DecafPosition}
                     deriving (Show, Eq)

instance ASTNode DecafMethodCall where
  pos (DecafPureMethodCall _ _ p) = p
  pos (DecafMethodCallout _ _ p) = p
  pp (DecafPureMethodCall identf args _) = "PureMethodcall[" ++ identf ++ "]"
  pp (DecafMethodCallout identf args _) = "MethodCallOut[" ++ identf ++"]"
  treeify meth@(DecafPureMethodCall identf args _) = (Node $ pp meth) (Just $ map treeify args)
  treeify meth@(DecafMethodCallout identf args _) = (Node $ pp meth) (Just $ map treeify args)

data DecafLoc = DecafVarLoc DecafIdentifier DecafPosition
              | DecafArrLoc DecafIdentifier DecafExpr DecafPosition
              deriving (Show, Eq)

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


data DecafCalloutArg = DecafCalloutArgExpr DecafExpr DecafPosition
                     | DecafCalloutArgStr DecafString DecafPosition
                     deriving (Show, Eq)

instance ASTNode DecafCalloutArg where
  pos (DecafCalloutArgExpr _ p) = p
  pos (DecafCalloutArgStr _ p) = p
  treeify (DecafCalloutArgExpr expr _) = treeify expr
  treeify (DecafCalloutArgStr str _) = (Node str) Nothing

data DecafExpr = DecafExpr Term Expr' DecafPosition -- used for parsing, but removed at tree rewrite
               | DecafLocExpr DecafLoc DecafPosition
               | DecafMethodExpr DecafMethodCall DecafPosition
               | DecafLitExpr DecafLiteral DecafPosition
               | DecafBinExpr DecafExpr DecafBinOp DecafExpr DecafPosition
               | DecafNotExpr DecafExpr DecafPosition
               | DecafMinExpr DecafExpr DecafPosition
               | DecafParenExpr DecafExpr DecafPosition
               deriving (Show, Eq)

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

data Expr' = Expr' DecafBinOp Term Expr' DecafPosition
           | EmptyExpr'
           deriving (Show, Eq)

instance ASTNode Expr' where
  pos (Expr' _ _ _ p) = p
  treeify (Expr' binop term expr' _) = (Node $ pp binop) (Just $ [treeify term] ++ [treeify expr'])
  treeify (EmptyExpr') = Nil

data Term = Term Factor Term' DecafPosition
          deriving (Show, Eq)

instance ASTNode Term where
  pos (Term _ _ p) = p
  treeify (Term factor term' _) = (Node "TERM") (Just $ [treeify factor] ++ [treeify term'])

data Term' = Term' DecafBinOp Factor Term' DecafPosition
           | EmptyTerm'
           deriving (Show, Eq)

instance ASTNode Term' where
  pos (Term' _ _ _ p) = p
  treeify (Term' binop factor term' _) = (Node $ pp binop) (Just $ [treeify factor] ++ [treeify term'])
  treeify (EmptyTerm') = Nil

data Factor = DecafParenExpr' DecafExpr DecafPosition
            | DecafNotExpr' DecafExpr DecafPosition
            | DecafMinExpr' DecafExpr DecafPosition
            | DecafLocExpr' DecafLoc DecafPosition
            | DecafMethodExpr' DecafMethodCall DecafPosition
            | DecafLitExpr' DecafLiteral DecafPosition
            deriving (Show, Eq)

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

data DecafBinOp = DecafBinArithOp DecafArithOp DecafPosition
                | DecafBinRelOp DecafRelOp DecafPosition
                | DecafBinEqOp DecafEqOp DecafPosition
                | DecafBinCondOp DecafCondOp DecafPosition
                deriving (Show, Eq)

instance ASTNode DecafBinOp where
  pos (DecafBinArithOp _ p) = p
  pos (DecafBinRelOp _ p) = p
  pos (DecafBinEqOp _ p) = p
  pos (DecafBinCondOp _ p) = p
  pp (DecafBinArithOp o _) = pp o
  pp (DecafBinRelOp o _) = pp o
  pp (DecafBinEqOp o _) = pp o
  pp (DecafBinCondOp o _) = pp o

data DecafArithOp = DecafPlusOp DecafPosition
                  | DecafMinOp DecafPosition
                  | DecafMulOp DecafPosition
                  | DecafDivOp DecafPosition
                  | DecafModOp DecafPosition
                  deriving (Show, Eq)

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

data DecafRelOp = DecafLTOp DecafPosition
                | DecafGTOp DecafPosition
                | DecafLTEOp DecafPosition
                | DecafGTEOp DecafPosition
                deriving (Show, Eq)

instance ASTNode DecafRelOp where
  pos (DecafLTOp p)  = p
  pos (DecafGTOp p)  = p
  pos (DecafGTEOp p) = p
  pos (DecafLTEOp p) = p
  pp (DecafLTOp _)  = "<"
  pp (DecafGTOp _)  = ">"
  pp (DecafGTEOp _) = ">="
  pp (DecafLTEOp _) = "<="
  
data DecafEqOp = DecafEqOp DecafPosition
               | DecafNEqOp DecafPosition
               deriving (Show, Eq)

instance ASTNode DecafEqOp where
  pos (DecafEqOp p)   = p
  pos (DecafNEqOp p) = p
  pp (DecafEqOp _)   = "=="
  pp (DecafNEqOp _) = "!="

data DecafCondOp = DecafAndOp DecafPosition
                 | DecafOrOp DecafPosition
                 deriving (Show, Eq)


instance ASTNode DecafCondOp where
  pos (DecafAndOp p) = p
  pos (DecafOrOp p)  = p
  pp (DecafAndOp _) = "&&"
  pp (DecafOrOp _)  = "||"

data DecafLiteral = DecafIntLit DecafInteger DecafPosition
               | DecafBoolLit Bool  DecafPosition
               | DecafStrLit  DecafString DecafPosition
               | DecafCharLit  DecafCharacter DecafPosition
               deriving (Show, Eq)

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

type DecafPosition = (Int, Int)
