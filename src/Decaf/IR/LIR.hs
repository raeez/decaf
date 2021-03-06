{-# LANGUAGE DeriveDataTypeable
  , TypeSynonymInstances #-}

module Decaf.IR.LIR where
--import Decaf.IR.Class
import Decaf.IR.IRNode
import Decaf.Data.Tree
import Decaf.IR.SymbolTable

import Numeric
import Data.Int
import Data.Typeable
--import Loligoptl.Label

type LIRInt    = Int64
type LIRSize   = LIRInt
type LIROffset = LIRInt

data LIRLabel = LIRLabel String Int
    deriving (Eq, Typeable, Ord)

data LIRProgram = LIRProgram
    { lirProgLabel :: LIRLabel
    , lirProgUnits :: [LIRUnit]
    } deriving (Show, Eq, Typeable)

data LIRUnit = LIRUnit
    { lirUnitLabel ::LIRLabel
    , lirUnitInstructions :: [LIRInst]
    } deriving (Show, Eq, Typeable)

data LIRInst = LIRRegAssignInst LIRReg LIRExpr
             | LIRRegOffAssignInst LIRReg LIRReg LIRSize LIROperand
             | LIRStoreInst LIRMemAddr LIROperand
             | LIRLoadInst LIRReg LIRMemAddr
             | LIREnterInst LIRInt
             | LIRJumpLabelInst LIRLabel
             | LIRIfInst LIRRelExpr LIRLabel LIRLabel -- false, then true
             | LIRCallInst LIRLabel LIRLabel -- method label, return label
             | LIRCalloutInst String
             | LIRRetInst [LIRLabel] String -- list of successors, and the name of the method returning from
             | LIRLabelInst LIRLabel
             deriving (Show, Eq, Typeable)


data LIRProc = LIRProcLabel LIRLabel
             | LIRProcReg LIRReg
             deriving (Show, Eq, Typeable)

data LIRExpr = LIRBinExpr LIROperand LIRBinOp LIROperand
             | LIRUnExpr LIRUnOp LIROperand
             | LIROperExpr LIROperand
             deriving (Show, Eq, Typeable)

data LIRRelExpr = LIRBinRelExpr LIROperand LIRRelOp LIROperand
                | LIRNotRelExpr LIROperand
                | LIROperRelExpr LIROperand
                deriving (Show, Eq, Typeable)

data LIRBinOp = LADD
              | LSUB
              | LMUL
              | LDIV
              | LMOD
              | LAND
              | LOR
              | LXOR
              | LSHL
              | LSHR
              | LSHRA
              | LIRBinRelOp LIRRelOp LIRLabel
              deriving (Show, Eq, Typeable, Ord)

data LIRUnOp = LNEG
             | LNOT
             deriving (Show, Eq, Typeable)

data LIRRelOp = LEQ
              | LNEQ
              | LGT
              | LGTE
              | LLT
              | LLTE
              deriving (Show, Eq, Typeable, Ord)

data LIRMemAddr = LIRMemAddr LIRReg (Maybe LIRReg) LIROffset LIRSize
                deriving (Show, Eq, Typeable)

data LIROperand = LIRRegOperand LIRReg
                | LIRIntOperand LIRInt
                | LIRStrOperand String
                deriving (Show, Eq, Typeable, Ord)

data LIRReg = LRAX
            | LRBX
            | LRCX
            | LRDX
            | LRBP
            | LRSP
            | LRSI
            | LRDI
            | LR8
            | LR9
            | LR10
            | LR11
            | LR12
            | LR13
            | LR14
            | LR15
            | GI Int
            | SREG Int
            | MEM String
            deriving (Show, Eq, Typeable, Ord)

-- | (very) Intermediate control flow graph representation
data CFGProgram = CFGProgram
    { cgProgLabel :: LIRLabel
    , cgProgUnits :: [CFGUnit]
    , cgProgLastLabel :: Int
    } deriving (Show, Eq)

data CFGUnit = CFGUnit
    { cgUnitLabel ::LIRLabel
    , cgUnitInstructions :: [CFGInst]
    } deriving (Show, Eq)

data CFGInst = CFGLIRInst LIRInst
             | CFGIf LIROperand JumpLabel [CFGInst] [CFGInst]
             | CFGExprInst { cgExpr :: CFGExpr }
               deriving (Show, Eq)

data CFGExpr = CFGLogExpr CFGInst LIRBinOp CFGInst LIRReg
             | CFGFlatExpr [CFGInst] LIROperand
              deriving (Show, Eq)

type JumpLabel = Int

cgOper (CFGExprInst (CFGLogExpr _ _ _ r)) = LIRRegOperand r
cgOper (CFGExprInst (CFGFlatExpr _ o)) = o
cgOper _ = error "Tried to mkBranch for improper CFGExprInst"


data ControlNode = BasicBlock [LIRInst]
                 | Branch
                   { condReg :: LIROperand
                   , falseBlock :: ControlPath
                   , trueBlock  :: ControlPath }

type ControlPath = [ControlNode]

data ControlGraph = ControlGraph {cgNodes :: [ControlPath]} -- a list of nodes for each method

byte :: LIRInt
byte = 1

word :: LIRInt
word = 2

dword :: LIRInt
dword = 4

qword :: LIRInt
qword = 8

asmTrue, asmFalse :: LIRInt
asmTrue = (-1)
asmFalse = 0

litTrue, litFalse :: LIRInt
litTrue = -1
litFalse = 0

missingRetMessage :: String
missingRetMessage = "*** RUNTIME ERROR ***: Missing return statement in method \"%s\"\n"

missingRet :: Int
missingRet = 0

outOfBoundsMessage :: String
outOfBoundsMessage = "*** RUNTIME ERROR ***: Array out of Bounds access in method \"%s\"\n"

outOfBounds :: Int
outOfBounds = 1

exception code
      | code == 1 = outOfBoundsMessage
      | code == 0 = missingRetMessage

exceptionHeader :: LIRLabel
exceptionHeader = LIRLabel "__exceptionhandlers" 0

exceptionString :: SymbolTree -> String -> String
exceptionString st msg =
  case globalSymLookup ('.':msg) st of
      Just (StringRec _ l) ->  "__string" ++ show l
      _                    ->
          error $ "LIR.hs:exceptionString could not find symbol for :" ++msg 

exceptionLabel :: SymbolTree -> String -> LIRLabel
exceptionLabel st msg  = case globalSymLookup ('.':msg) st of
                           Just (StringRec _ l) ->  LIRLabel "__exception" (-l-100) -- if it's not negative, we'll clash with the universe of labels
                           _ -> error $ "LIR.hs:exceptionLabel could not find symbol for :" ++ msg

boundsLabel :: Bool -> Int -> LIRLabel
boundsLabel t c = if t then (LIRLabel  "__boundscheck" c) 
                  else (LIRLabel "__boundscheck_fail" c)

compareLabel :: Int -> LIRLabel
compareLabel c = LIRLabel  "__cmp" c


-- following three are not labels in the LIRLabel sense
stringLabel :: Int -> String
stringLabel c = "__string" ++ show c

arrayLabel :: Int -> String
arrayLabel c = "__array" ++ show c

methodLabel :: String -> Int -> LIRLabel
methodLabel m c = LIRLabel ("__proc" ++ "__" ++ m) c

loopLabel :: Int -> LIRLabel
loopLabel l = LIRLabel "LLOOP" l

endLabel :: Int -> LIRLabel
endLabel l = LIRLabel "LEND"  l

trueLabel :: Int -> LIRLabel
trueLabel l = LIRLabel "LTRUE" l

uniqueID :: LIRLabel -> Int
uniqueID (LIRLabel _ i) = i

instance Show LIRLabel where
  show (LIRLabel s n) = if n == -1 then s else s ++ (show n)

falseLabel :: Int -> LIRLabel
falseLabel l = LIRLabel "LFalse" l

instance IRNode LIRProgram where
    pp (LIRProgram label units) = pp label ++ ":\n" ++ unlines (map pp units)
    treeify (LIRProgram label units) = Node (pp label) (map treeify units)

instance IRNode LIRUnit where
    pp (LIRUnit label insts) =
        "\n" ++ pp label ++ ":\n" ++ unlines (indentMap insts)
      where
        indentMap [] = []
        indentMap (x:xs) = let repr = case x of
                                    LIRLabelInst _ -> "\n" ++ pp x ++ ":"
                                    _ -> "    " ++ pp x
                        in (repr:indentMap xs)
    treeify (LIRUnit label insts) = Node ("LIRUnit: " ++ pp label) (map treeify insts)

instance IRNode LIRInst where
    pp (LIRRegAssignInst reg expr) = pp reg ++ " <- " ++ pp expr
    pp (LIRRegOffAssignInst reg offset size operand) =
        pp reg ++ "(" ++ pp offset ++ ", " ++ pp size ++ ") <- " ++ pp operand
    pp (LIRStoreInst mem operand) = "STORE " ++ pp mem ++ ", " ++ pp operand
    pp (LIRLoadInst reg mem) = "LOAD " ++ pp reg ++ ", " ++ pp mem
    pp (LIREnterInst num) = "ENTER " ++ (show num)
    pp (LIRJumpLabelInst label) = "JMP " ++ pp label
    pp (LIRIfInst expr flab tlab) = "IF " ++ pp expr ++ " THEN " ++ pp tlab ++ " ELSE " ++ pp flab
    pp (LIRCallInst proc ret) = "CALL " ++ pp proc ++ " AND RETURN TO " ++ pp ret
    pp (LIRCalloutInst proc) = "CALL " ++ proc
    pp (LIRRetInst _ _) = "RET"
    pp (LIRLabelInst label) = "LABEL: " ++ pp label
    treeify (LIRRegAssignInst reg expr) =
        Node "ASSIGN" [treeify reg, treeify expr]
    treeify (LIRRegOffAssignInst reg offset size operand) =
        Node "ASSIGN" $ [treeify reg, treeify offset, treeify size, treeify operand]
    treeify (LIRStoreInst mem operand) =
        Node "STR" [treeify mem, treeify operand]
    treeify (LIRLoadInst reg mem) = Node "LD" [treeify reg, treeify mem]
    treeify (LIREnterInst bytes) = Node ("Enter[" ++ pp bytes ++ "]") []
    treeify (LIRJumpLabelInst label) = Node "JMP" [treeify label]
    treeify (LIRIfInst expr flab tlab) = Node "IF" [treeify expr, treeify flab, treeify tlab]
    treeify (LIRCallInst proc ret) = Node "CALL" [treeify proc]
    treeify (LIRRetInst _ _) = Node "RET" []
    treeify (LIRLabelInst label) = Node (pp label) []

instance IRNode LIRProc where
    pp (LIRProcLabel label) = pp label
    pp (LIRProcReg reg) = pp reg
    treeify (LIRProcLabel label) = Node (pp label) []
    treeify (LIRProcReg reg) = treeify reg

instance IRNode LIRExpr where
    pp (LIRBinExpr operand binop operand') =
        pp operand ++ " " ++ pp binop ++ " " ++ pp operand'
    pp (LIRUnExpr unop operand) = pp unop ++ pp operand
    pp (LIROperExpr operand) = pp operand
    treeify (LIRBinExpr operand binop operand') =
        Node (pp binop) [treeify operand, treeify operand']
    treeify (LIRUnExpr unop operand) = Node (pp unop) [treeify operand]
    treeify (LIROperExpr operand) = Node (pp operand) []

instance IRNode LIRRelExpr where
    pp (LIRBinRelExpr operand relop operand') =
        pp operand ++ " " ++ pp relop ++ " " ++ pp operand'
    pp (LIRNotRelExpr operand) = "!" ++ pp operand
    pp (LIROperRelExpr operand) = pp operand
    treeify (LIRBinRelExpr operand relop operand') =
        Node (pp relop) [treeify operand, treeify operand']
    treeify (LIRNotRelExpr operand) = Node "!" [treeify operand]
    treeify (LIROperRelExpr operand) = Node (pp operand) []

instance IRNode LIRBinOp where
    pp (LADD) = "ADD"
    pp (LSUB) = "SUB"
    pp (LMUL) = "MUL"
    pp (LDIV) = "DIV"
    pp (LMOD) = "MOD"
    pp (LAND) = "AND"
    pp (LOR) = "OR"
    pp (LXOR) = "XOR"
    pp (LSHL) = "SHL"
    pp (LSHR) = "SHR"
    pp (LSHRA) = "SHRA"
    pp (LIRBinRelOp relop _) = pp relop
    treeify (LIRBinRelOp relop _) = treeify relop
    treeify a = Node (pp a) []

instance IRNode LIRUnOp where
    pp (LNEG) = "-"
    pp (LNOT) = "!"
    treeify a = Node (pp a) []

instance IRNode LIRRelOp where
    pp (LEQ) = "=="
    pp (LNEQ) = "!="
    pp (LGT) = ">"
    pp (LGTE) = ">="
    pp (LLT) = "<"
    pp (LLTE) = "<="
    treeify a = Node (pp a) []

instance IRNode LIRMemAddr where
    pp (LIRMemAddr base reg offset _) =
        "[" ++ pp base ++ " + " ++ reg' ++ " + " ++ pp offset ++ "]"
      where
        reg' = case reg of
                  Just r -> pp r
                  Nothing -> ""

    treeify (LIRMemAddr base reg offset size) =
        Node "MemAddr" $ [treeify base] ++ reg' ++ [treeify offset, treeify size]
      where
        reg' = case reg of
                  Just r -> [treeify r]
                  Nothing -> []

instance IRNode LIROperand where
    pp (LIRRegOperand reg) = pp reg
    pp (LIRIntOperand i) = pp i
    pp (LIRStrOperand s) = s
    treeify a = Node (pp a) []

instance IRNode LIRReg where
    pp (LRAX) = "RAX"
    pp (LRBX) = "RBX"
    pp (LRCX) = "RCX"
    pp (LRDX) = "RDX"
    pp (LRBP) = "RBP"
    pp (LRSP) = "RSP"
    pp (LRSI) = "RSI"
    pp (LRDI) = "RDI"
    pp (LR8)  = "R8"
    pp (LR9)  = "R9"
    pp (LR10) = "R10"
    pp (LR11) = "R11"
    pp (LR12) = "R12"
    pp (LR13) = "R13"
    pp (LR14) = "R14"
    pp (LR15) = "R15"
    pp (GI i) = "g"++(show i)
    pp (MEM s)  = s
    pp (SREG i) = "s" ++ (show i)
    treeify a = Node (pp a) []

instance IRNode LIRInt where
    pp i = (if i < 0 then "-" else "") ++ "0x" ++ showHex (abs i) ""
    treeify i = Node (pp i) []

instance IRNode LIRLabel where
    pp l@(LIRLabel s n) = show l
    treeify s = Node (pp s) []
