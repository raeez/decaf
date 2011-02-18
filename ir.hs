module IR
where

data DecafProgram = DecafProgram [DecafField] [DecafMethod]
                  deriving (Show, Eq)

data DecafField = DecafVarField DecafVar
               | DecafArrField DecafArr
               deriving (Show, Eq)

data DecafMethod = DecafMethod DecafType DecafIdentifier [DecafVar] DecafBlock
                 deriving (Show, Eq)

data DecafVar = DecafVar DecafType DecafIdentifier
                  deriving (Show, Eq)

data DecafBlock = DecafBlock [DecafVar] [DecafStm]
                deriving (Show, Eq)

data DecafArr = DecafArr DecafType DecafIdentifier DInt
                  deriving (Show, Eq)

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

data DecafMethodCall = DecafMethodCall DecafIdentifier [DecafExpr]
                     | DecafMethodCallout DStr [DecafCalloutArg]
                     deriving (Show, Eq)

data DecafLoc = DecafVarLoc DecafIdentifier
              | DecafArrLoc DecafIdentifier DecafExpr
              deriving (Show, Eq)

data DecafExpr = DecafExpr' DecafExpr' (Maybe [(DecafBinOp, DecafExpr')])
               deriving (Show, Eq)

data DecafExpr' = DecafTermExpr DecafTermExpr
                | DecafParenExpr DecafExpr
                | DecafNotExpr DecafExpr'
                | DecafMinExpr DecafExpr'
                deriving (Show, Eq)

data DecafTermExpr = DecafLocExpr DecafLoc
                   | DecafMethodExpr DecafMethodCall
                   | DecafLitExpr DecafLiteral
                   deriving (Show, Eq)

data DecafCalloutArg = DecafCalloutArgExpr DecafExpr
                     | DecafCalloutArgStr DStr
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
