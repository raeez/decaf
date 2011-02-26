module Decaf.AST
where

-- entire program
data DecafProgram = DecafProgram {
    fields :: [DecafField],
    methods :: [DecafMethod]
} deriving (Show, Eq)

-- field declaration :: Variable or Array
data DecafField = DecafVarField DecafVar
               | DecafArrField DecafArray
               deriving (Show, Eq)

-- method declaration
data DecafMethod = DecafMethod {
    methodType :: DecafType,
    methodID :: DecafIdentifier,
    methodArg :: [DecafVar],
    methodBody :: DecafBlock
} deriving (Show, Eq)

-- variable declaration
data DecafVar = DecafVar {
    varType :: DecafType,
    varID :: DecafIdentifier
} deriving (Show, Eq)

-- array declaration
data DecafArray = DecafArray {
    arrayType :: DecafType,
    arrayID :: DecafIdentifier,
    arrayLength :: DInt
} deriving (Show, Eq)

-- block body
data DecafBlock = DecafBlock {
    blockVars :: [DecafVar],
    blockStms :: [DecafStm]
} deriving (Show, Eq)

-- types : consider differentiating the two kinds of types
-- (one is an expression type (int|bool) and the other is method return type (int|bool|void))
data DecafType = DInteger
               | DBoolean
               | DVoid
               deriving (Show, Eq)

data DecafStm = DecafAssignStm DecafLoc DecafAssignOp DecafExpr
              | DecafMethodStm DecafMethodCall
              | DecafIfStm DecafExpr DecafBlock (Maybe DecafBlock)
              | DecafForStm DecafIdentifier DecafExpr DecafExpr DecafBlock
              | DecafRetStm (Maybe DecafExpr)
              | DecafBreakStm
              | DecafContStm
              | DecafBlockStm DecafBlock
              deriving (Show, Eq)

data DecafAssignOp = DecafEq
                   | DecafPlusEq
                   | DecafMinusEq
                   deriving (Show, Eq)

data DecafMethodCall = DecafPureMethodCall { methodCallID :: DecafIdentifier, methodCallArgs :: [DecafExpr] }
                     | DecafMethodCallout { methodCalloutID :: DStr, methodCalloutArgs :: [DecafCalloutArg] }
                     deriving (Show, Eq)

data DecafLoc = DecafVarLoc DecafIdentifier
              | DecafArrLoc DecafIdentifier DecafExpr
              deriving (Show, Eq)

data DecafCalloutArg = DecafCalloutArgExpr DecafExpr
                     | DecafCalloutArgStr DStr
                     deriving (Show, Eq)

data DecafExpr = DecafExpr Term Expr' -- used for parsing, but removed at tree rewrite
               | DecafLocExpr DecafLoc
               | DecafMethodExpr DecafMethodCall
               | DecafLitExpr DecafLiteral
               | DecafBinExpr DecafBinOp DecafExpr
               | DecafNotExpr DecafExpr
               | DecafMinExpr DecafExpr
               | DecafParenExpr DecafExpr
               deriving (Show, Eq)

data Expr' = Expr' DecafBinOp Term Expr'
           | EmptyExpr'
           deriving (Show, Eq)

data Term = Term Factor Term'
          deriving (Show, Eq)

data Term' = Term' DecafBinOp Factor Term'
           | EmptyTerm'
           deriving (Show, Eq)

data Factor = DecafParenExpr' DecafExpr
            | DecafNotExpr' DecafExpr
            | DecafMinExpr' DecafExpr
            | DecafLocExpr' DecafLoc
            | DecafMethodExpr' DecafMethodCall
            | DecafLitExpr' DecafLiteral
            deriving (Show, Eq)

data DecafBinOp = DecafBinArithOp DecafArithOp
                | DecafBinRelOp DecafRelOp
                | DecafBinEqOp DecafEqOp
                | DecafBinCondOp DecafCondOp
                deriving (Show, Eq)

data DecafArithOp = DecafPlusOp
                  | DecafMinOp
                  | DecafMulOp
                  | DecafDivOp
                  | DecafModOp
                  deriving (Show, Eq)

data DecafRelOp = DecafLTOp
                | DecafGTOp
                | DecafLTEOp
                | DecafGTEOp
                deriving (Show, Eq)

data DecafEqOp = DecafEqOp
               | DecafNEqOp
               deriving (Show, Eq)

data DecafCondOp = DecafAndOp
                 | DecafOrOp
                 deriving (Show, Eq)

data DecafIdentifier = DecafIdentifier String
                     | DecafKeyword String
                     deriving (Show, Eq)

data DecafLiteral = DecafIntLit DInt
               | DecafBoolLit Bool 
               | DecafStrLit  DStr
               | DecafCharLit  DChar
               deriving (Show, Eq)

data DStr = DStr String
          deriving (Show, Eq)

data DInt = DDec String
          | DHex  String
          deriving (Show, Eq)

data DBool = DTrue 
           | DFalse
           deriving (Show, Eq)

data DChar = DChar Char
           deriving (Show, Eq)

-- spaces
gap = 5
put_spaces n = (map (\_ -> ' ') [1..n])

-- expr
put_expr n (DecafExpr term expr') = (put_spaces n) ++ "-->Expr\n" ++ (put_term (n+gap) term) ++ (put_expr' (n+gap) expr')

-- expr'
put_expr' n (Expr' binop term expr') = (put_binop n binop) ++ (put_term (n+gap) term) ++ (put_expr' (n+gap) expr')
put_expr' n (EmptyExpr') = (put_spaces n) ++ "-->NULL\n"

-- binary operations
put_binop n (DecafBinArithOp (DecafPlusOp)) = (put_spaces n) ++ "+\n"
put_binop n (DecafBinArithOp (DecafMinOp)) = (put_spaces n) ++ "-\n"
put_binop n (DecafBinArithOp (DecafMulOp)) = (put_spaces n) ++ "*\n"
put_binop n (DecafBinArithOp (DecafDivOp)) = (put_spaces n) ++ "/\n"
put_binop n (DecafBinArithOp (DecafModOp)) = (put_spaces n) ++ "/\n"
put_binop n (DecafBinRelOp (DecafLTOp)) = (put_spaces n) ++ "<\n"
put_binop n (DecafBinRelOp (DecafGTOp)) = (put_spaces n) ++ ">\n"
put_binop n (DecafBinRelOp (DecafLTEOp)) = (put_spaces n) ++ "<=\n"
put_binop n (DecafBinRelOp (DecafGTEOp)) = (put_spaces n) ++ ">=\n"
put_binop n (DecafBinEqOp (DecafEqOp)) = (put_spaces n) ++ "==\n"
put_binop n (DecafBinEqOp (DecafNEqOp)) = (put_spaces n) ++ "==\n"
put_binop n (DecafBinCondOp (DecafAndOp)) = (put_spaces n) ++ "&&\n"
put_binop n (DecafBinCondOp (DecafOrOp)) = (put_spaces n) ++ "||\n"
put_binop n _ = (put_spaces n) ++ "___\n"
-- do the other operators

--term
put_term n (Term factor term')= (put_spaces n) ++ "-->Term\n" ++ (put_factor (n+gap) factor) ++ (put_term' (n+gap) term')

--term'
put_term' n (Term' binop factor term') = (put_binop n binop) ++ (put_factor (n+gap) factor) ++ (put_term' (n+gap) term')
put_term' n (EmptyTerm') = (put_spaces n) ++ "-->NULL\n"

--factor
put_factor n (DecafParenExpr' dpe) = (put_spaces n) ++ "(  )\n" ++ (put_expr (n+gap) dpe)
put_factor n (DecafNotExpr' dne) = (put_spaces n) ++ "!\n" ++ (put_expr (n+gap) dne)
put_factor n (DecafMinExpr' dme) = (put_spaces n) ++ "-\n" ++ (put_expr (n+gap) dme)
put_factor n (DecafLocExpr' dle) = (put_spaces n) ++ "-->LOCATION\n"
put_factor n (DecafMethodExpr' dme) = (put_spaces n) ++ "-->METHODCALL\n"
put_factor n (DecafLitExpr' dl) = (put_spaces n) ++ "-->LITERAL\n"

-- all of them
--flatten n (DecafExpr term expr') = []
--flatten n (Expr' binop term expr') = []
--flatten n (EmptyExpr') = []
--flatten n (Term factor term') = []
--flatten n (Term' binop factor term) = []
--flatten n (EmptyTerm') = []
--flatten n (DecafParenExpr' de) = []
--flatten n (DecafNotExpr' dne) = []
--flatten n (DecafMinExpr' dme) = []
--flatten n (DecafLocExpr' dle) = []
--flatten n (DecafMethodExpr' dme) = []
--flatten n (DecafLitExpr' dle) = []
--flatten n (DecafBinArithOp dbo) = []
--flatten n (DecafBinRelOp dbo) = []
--flatten n (DecafBinEqOp dbo) = []
--flatten n (DecafBinCondOp dco) = []
--flatten n (DecafPlusOp dpo) = []
--flatten n (DecafMinOp dmo) = []
--flatten n (DecafMulOp dmo) = []
--flatten n (DecafDivOp dvo) = []
--flatten n (DecafModOp dmo) = []
--flatten n (DecafLTOp dlto) = []
--flatten n (DecafGTOp dgto) = []
--flatten n (DecafLTEOp dlteo) = []
--flatten n (DecafGTEOp dgteo) = []
--flatten n (DecafEqOp deo) = []
--flatten n (DecafNEqOp dneo) = []
--flatten n (DecafAndOp dao) = []
--flatten n (DecafOrOp doo) = []
