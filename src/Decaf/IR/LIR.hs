module Decaf.IR.LIR where
import Data.Int
import Decaf.IR.Class

newtype LIRProgram = LIRProgram {program :: [LIRUnit] }
                   deriving (Show, Eq)

newtype LIRUnit = LIRUnit { instructions :: [LIRInst] }
                deriving (Show, Eq)

data LIRInst = LIRRegAssignInst LIRReg LIRExpr
             | LIRRegOffAssignInst LIRReg LIROffset LIRSize LIROperand  -- Element-wise Assign
             | LIRCondAssignInst LIRReg LIRReg LIROperand    -- Conditional Assign
             | LIRStoreInst LIRMemAddr LIROperand
             | LIRLoadInst LIRReg LIRMemAddr
             | LIRJumpRegInst LIRReg LIROffset
             | LIRJumpLabelInst LIRLabel
             | LIRIfInst LIRRelExpr LIRLabel
             | LIRCallInst LIRCall
             | LIRRetOperInst LIROperand
             | LIRRetInst
             | LIRLabelInst LIRLabel LIRInst
             deriving (Show, Eq)

data LIRCall = LIRCallAssign LIRReg LIRProc LIRReg -- last reg is where to store retaddr
             | LIRCall LIRProc LIRReg              -- last reg is where to store retaddr
             deriving (Show, Eq)

data LIRProc = LIRProcLabel LIRLabel
             | LIRProcReg LIRReg
             deriving (Show, Eq)

data LIRExpr = LIRBinExpr LIROperand LIRBinOp LIROperand
             | LIRUnExpr LIRUnOp LIROperand
             | LIROperExpr LIROperand
             deriving (Show, Eq)

data LIRRelExpr = LIRBinRelExpr LIROperand LIRRelOp LIROperand
                | LIRNotRelExpr LIROperand
                | LIROperRelExpr LIROperand
                deriving (Show, Eq)

data LIRBinOp = LADD
              | LMIN
              | LMUL
              | LDIV
              | LMOD
              | LAND
              | LOR
              | LXOR
              | LSHL
              | LSHR
              | LSHRA
              | LIRBinRelOp LIRRelOp
              deriving (Show, Eq)

data LIRUnOp = LNEG
             | LNOT
             deriving (Show, Eq)

data LIRRelOp = LEQ
              | LNEQ
              | LGT
              | LGTE
              | LLT
              | LLTE
              deriving (Show, Eq)

data LIRMemAddr = LIRRegMemAddr LIRReg LIRSize
                | LIRReg2MemAddr LIRReg LIRReg LIRSize
                | LIRRegOffMemAddr LIRReg LIROffset LIRSize
                deriving (Show, Eq)

data LIROperand = LIRRegOperand LIRReg
                | LIRIntOperand LIRInt
                deriving (Show, Eq)

data LIRReg = RAX
            | RBX
            | RCX
            | RDX
            | RBP
            | RSP
            | RSI
            | RDI
            | R8
            | R9
            | R10
            | R11
            | R12
            | R13
            | R14
            | R15
            | SREG Int64
            deriving (Show, Eq)

type LIRSize = LIRInt
type LIROffset = LIRInt
type LIRInt = Int64
type LIRLabel = String

instance IRNode LIRProgram where
    pp (LIRProgram units) = unlines (map pp units)
    treeify (LIRProgram units) = Node "LIRProgram" (map treeify units)

instance IRNode LIRUnit where
    pp (LIRUnit insts) = unlines (map pp insts)
    treefiy (LIRUnit insts) = Node "LIRUnit" (map treeify insts)

instance IRNode LIRInst where
    pp (LIRRegAssignInst reg expr) = pp reg ++ " <- " ++ pp reg
    pp (LIRRegOffAssignInst reg offset size operand) = pp reg ++ "(" ++ pp offset ++ ", " ++ pp size ++ ") <- " ++ pp operand
    pp (LIRCondAssignInst reg reg operand) = pp reg ++ " <- (" ++ pp reg ++ ") " ++ pp operand
    pp (LIRStoreInst mem operand) "STORE " ++ pp mem ++ ", " ++ pp operand
    pp (LIRLoadInst reg mem) = "LOAD " ++ pp reg ++ ", " ++ pp mem
    pp (LIRJumpRegInst reg offset) = "JMP " ++ pp reg ++ "[" ++ show offset ++ "]"
    pp (LIRJumpLabelInst label) = "JMP " ++ pp label
    pp (LIRIfInst expr label) = "IF " ++ pp expr ++ " JMP " ++ pp label
    pp (LIRCallInst call) = pp call
    pp (LIRRetOperInst operand) = "RET " ++ pp operand
    pp LIRRetInst = "RET"
    pp (LIRLabelInst label inst) = "\n    L" ++ show label ++ "\n" ++ pp (inst)
    treeify (LIRRegAssignInst reg expr) = Node "ASSIGN" [treeify reg, treeify expr]
    treeify (LIRRegOffAssignInst reg offset size operand) Node "ASSIGN" [treeify reg, treeify offset, treeify size, treeify operand]
    treeify (LIRCondAssignInst reg reg operand) = Node "CONDASSIGN" [treeify reg, treeify reg, treeify operand]
    treeify (LIRStoreInst mem operand) = Node "STR" [treeify mem, treeify operand]
    treeify (LIRLoadInst reg mem) = Node "LD" [treeify reg, treeify mem]
    treeify (LIRJumpRegInst reg offset) = Node "JMP" [treefiy reg, treeify offset]
    treeify (LIRJumpLabelInst label) = Node "JMP" [treeify label]
    treeify (LIRIfInst expr label) = Node "IF" [treeify expr, treeify label]
    treeify (LIRCallInst call) = Node "CALL" [treeify call]
    treeify (LIRRetOperInst operand) = Node "RET" [treeify operand]
    treeify LIRRetInst = Node "RET" []
    treeify (LIRLabelInst label inst) = Node (pp label) [treeify inst]

instance IRNode LIRCall where
    pp (LIRCallAssign LIRReg LIRProc LIRReg) -- last reg is where to store retaddr
    pp (LIRCall LIRProc LIRReg)              -- last reg is where to store retaddr

instance IRNode LIRProc where
    pp (LIRProcLabel LIRLabel)
    pp (LIRProcReg LIRReg)

instance LIRExpr where
    pp (LIRBinExpr LIROperand LIRBinOp LIROperand)
    pp (LIRUnExpr LIRUnOp LIROperand)
    pp (LIROperExpr LIROperand)

instance IRNode LIRRelExpr where
    pp (LIRBinRelExpr LIROperand LIRRelOp LIROperand)
    pp (LIRNotRelExpr LIROperand)
    pp (LIROperRelExpr LIROperand)

instance IRNode LIRBinOp where
    pp (LADD)
    pp (LMIN)
    pp (LMUL)
    pp (LDIV)
    pp (LMOD)
    pp (LAND)
    pp (LOR)
    pp (LXOR)
    pp (LSHL)
    pp (LSHR)
    pp (LSHRA)
    pp (LIRBinRelOp LIRRelOp)

instance IRNode LIRUnOp where
    pp (LNEG)
    pp (LNOT)

instance IRNode LIRRelOp where
    pp (LNEQ)
    pp (LGT)
    pp (LGTE)
    pp (LLT)
    pp (LLTE)

instance IRNode LIRMemAddr where
    pp (LIRRegMemAddr LIRReg LIRSize)
    pp (LIRSizeLIRReg2MemAddr LIRReg LIRReg LIRSize)
    pp (LIRRegOffMemAddr LIRReg LIROffset LIRSize)

instance IRNode LIROperand where
    pp (LIRRegOperand LIRReg)
    pp (LIRIntOperand LIRInt)

instance IRNode LIRReg where
    pp (RAX)
    pp (RBX)
    pp (RCX)
    pp (RDX)
    pp (RBP)
    pp (RSP)
    pp (RSI)
    pp (RDI)
    pp (R8)
    pp (R9)
    pp (R10)
    pp (R11)
    pp (R12)
    pp (R13)
    pp (R14)
    pp (R15)
    pp (SREG Int64)
