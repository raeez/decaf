module Decaf.IR.LIR where
import Data.Int
import Decaf.IR.Class

newtype LIRProgram = LIRProgram {program :: [LIRUnit] }
                   deriving (Show, Eq)

newtype LIRUnit = LIRUnit { instructions :: [LIRInst] }
                deriving (Show, Eq)

data LIRInst = LIRRegAssignInst LIRReg LIRExpr
             | LIRRegOffAssignInst LIRReg LIROffset LIRSize  -- Element-wise Assign
             | LIRCondAssignInst LIRReg LIRReg LIROperand    -- Conditional Assign
             | LIRStoreInst LIRMemAddr LIROperand
             | LIRLoadInst LIRReg LIRMemAddr
             | LIRJumpRegInst LIRReg LIROffset
             | LIRJumpLabelInst LIRLabel
             | LIRIfInst LIRRelExpr LIRLabel
             | LIRCallInst LIRCall
             | LIRRetInst LIROperand
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

data LIROperand = LIRReg
                | LIRInt
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
