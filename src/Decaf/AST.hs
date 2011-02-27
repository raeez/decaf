module Decaf.AST
where

-- entire program
data DecafProgram = DecafProgram {
    fields :: [DecafField],
    methods :: [DecafMethod]
} deriving (Show, Eq)

-- field declaration :: Variable or Array
data DecafField = DecafVarField DecafVar
               | DecafArrField DecafArr
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
data DecafArr = DecafArr {
    arrayType :: DecafType,
    arrayID :: DecafIdentifier,
    arrayLength :: DecafInteger
} deriving (Show, Eq)

-- block body
data DecafBlock = DecafBlock {
    blockVars :: [DecafVar],
    blockStms :: [DecafStm]
} deriving (Show, Eq)

-- types : consider differentiating the two kinds of types
-- (one is an expression type (int|bool) and the other is method return type (int|bool|void))
data DecafType = DecafInteger
               | DecafBoolean
               | DecafVoid
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
                     | DecafMethodCallout { methodCalloutID :: DecafString, methodCalloutArgs :: [DecafCalloutArg] }
                     deriving (Show, Eq)

data DecafLoc = DecafVarLoc DecafIdentifier
              | DecafArrLoc DecafIdentifier DecafExpr
              deriving (Show, Eq)

data DecafCalloutArg = DecafCalloutArgExpr DecafExpr
                     | DecafCalloutArgStr DecafString
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

type DecafIdentifier = String

data DecafLiteral = DecafIntLit DecafInteger
               | DecafBoolLit Bool 
               | DecafStrLit  DecafString
               | DecafCharLit  DecafCharacter
               deriving (Show, Eq)

type DecafString = String

data DecafInteger = DecafDec String
                  | DecafHex  String
                  deriving (Show, Eq)

data DecafBoolean = DecafTrue 
                  | DecafFalse
                  deriving (Show, Eq)

type DecafCharacter = Char

-- spaces
gap = 5
pretty_print_spaces n = (map (\_ -> ' ') [1..n])

-- expr
pretty_print_expr n (DecafExpr term expr') = (pretty_print_spaces n) ++ "-->Expr\n" ++ (pretty_print_term (n+gap) term) ++ (pretty_print_expr' (n+gap) expr')

-- expr'
pretty_print_expr' n (Expr' binop term expr') = (pretty_print_binop n binop) ++ (pretty_print_term (n+gap) term) ++ (pretty_print_expr' (n+gap) expr')
pretty_print_expr' n (EmptyExpr') = (pretty_print_spaces n) ++ "-->NULL\n"

-- binary operations
pretty_print_binop n (DecafBinArithOp (DecafPlusOp)) = (pretty_print_spaces n) ++ "+\n"
pretty_print_binop n (DecafBinArithOp (DecafMinOp)) = (pretty_print_spaces n) ++ "-\n"
pretty_print_binop n (DecafBinArithOp (DecafMulOp)) = (pretty_print_spaces n) ++ "*\n"
pretty_print_binop n (DecafBinArithOp (DecafDivOp)) = (pretty_print_spaces n) ++ "/\n"
pretty_print_binop n (DecafBinArithOp (DecafModOp)) = (pretty_print_spaces n) ++ "/\n"
pretty_print_binop n (DecafBinRelOp (DecafLTOp)) = (pretty_print_spaces n) ++ "<\n"
pretty_print_binop n (DecafBinRelOp (DecafGTOp)) = (pretty_print_spaces n) ++ ">\n"
pretty_print_binop n (DecafBinRelOp (DecafLTEOp)) = (pretty_print_spaces n) ++ "<=\n"
pretty_print_binop n (DecafBinRelOp (DecafGTEOp)) = (pretty_print_spaces n) ++ ">=\n"
pretty_print_binop n (DecafBinEqOp (DecafEqOp)) = (pretty_print_spaces n) ++ "==\n"
pretty_print_binop n (DecafBinEqOp (DecafNEqOp)) = (pretty_print_spaces n) ++ "==\n"
pretty_print_binop n (DecafBinCondOp (DecafAndOp)) = (pretty_print_spaces n) ++ "&&\n"
pretty_print_binop n (DecafBinCondOp (DecafOrOp)) = (pretty_print_spaces n) ++ "||\n"

--term
pretty_print_term n (Term factor term')= (pretty_print_spaces n) ++ "-->Term\n" ++ (pretty_print_factor (n+gap) factor) ++ (pretty_print_term' (n+gap) term')

--term'
pretty_print_term' n (Term' binop factor term') = (pretty_print_binop n binop) ++ (pretty_print_factor (n+gap) factor) ++ (pretty_print_term' (n+gap) term')
pretty_print_term' n (EmptyTerm') = (pretty_print_spaces n) ++ "-->NULL\n"

--factor
pretty_print_factor n (DecafParenExpr' dpe) = (pretty_print_spaces n) ++ "(  )\n" ++ (pretty_print_expr (n+gap) dpe)
pretty_print_factor n (DecafNotExpr' dne) = (pretty_print_spaces n) ++ "!\n" ++ (pretty_print_expr (n+gap) dne)
pretty_print_factor n (DecafMinExpr' dme) = (pretty_print_spaces n) ++ "-\n" ++ (pretty_print_expr (n+gap) dme)
pretty_print_factor n (DecafLocExpr' dle) = (pretty_print_spaces n) ++ "-->LOCATION\n"
pretty_print_factor n (DecafMethodExpr' dme) = (pretty_print_spaces n) ++ "-->METHODCALL\n"
pretty_print_factor n (DecafLitExpr' dl) = (pretty_print_spaces n) ++ "-->LITERAL\n"

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
--
--
