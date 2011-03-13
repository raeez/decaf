module Decaf.IR.LIR where
import Numeric
import Decaf.IR.Class
import Decaf.Data.Tree

data LIRProgram = LIRProgram
    { progLabel :: LIRLabel
    , progUnits :: [LIRUnit]
    } deriving (Show, Eq)

data LIRUnit = LIRUnit
    { unitLabel ::LIRLabel
    , unitInstructions :: [LIRInst]
    } deriving (Show, Eq)

data LIRInst = LIRRegAssignInst LIRReg LIRExpr
             | LIRRegOffAssignInst LIRReg LIROffset LIRSize LIROperand  -- ^ Element-wise Assign
             | LIRCondAssignInst LIRReg LIRReg LIROperand    -- ^ Conditional Assign
             | LIRStoreInst LIRMemAddr LIROperand
             | LIRLoadInst LIRReg LIRMemAddr
             | LIRJumpRegInst LIRReg LIROffset
             | LIRJumpLabelInst LIRLabel
             | LIRIfInst LIRRelExpr LIRLabel
             | LIRCallInst LIRCall
             | LIRRetOperInst LIROperand
             | LIRRetInst
             | LIRLabelInst LIRLabel
             deriving (Show, Eq)

data LIRCall = LIRCallAssign LIRReg LIRProc LIRReg -- ^ last reg is where to store retaddr
             | LIRCall LIRProc LIRReg              -- ^ last reg is where to store retaddr
             deriving (Show, Eq)

data LIRProc = LIRProcLabel String
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
                | LIRRegPlusMemAddr LIRReg LIRReg LIRSize
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
            | SREG String
            deriving (Show, Eq)

type LIRSize = LIRInt

type LIROffset = LIRInt

data LIRInt = LIRInt Int
            deriving (Show, Eq)

data LIRLabel = LIRLabel String
              deriving (Show, Eq)

instance IRNode LIRProgram where
    pp (LIRProgram label units) = pp label ++ ":\n" ++ unlines (map pp units)
    treeify (LIRProgram label units) = Node (pp label) (map treeify units)
    pos _     = error "LIR has no associated position"

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
    pos _     = error "LIR has no associated position"

instance IRNode LIRInst where
    pp (LIRRegAssignInst reg expr) = pp reg ++ " <- " ++ pp expr
    pp (LIRRegOffAssignInst reg offset size operand) = pp reg ++ "(" ++ pp offset ++ ", " ++ pp size ++ ") <- " ++ pp operand
    pp (LIRCondAssignInst reg reg' operand) = pp reg ++ " <- (" ++ pp reg' ++ ") " ++ pp operand
    pp (LIRStoreInst mem operand) = "STORE " ++ pp mem ++ ", " ++ pp operand
    pp (LIRLoadInst reg mem) = "LOAD " ++ pp reg ++ ", " ++ pp mem
    pp (LIRJumpRegInst reg offset) = "JMP " ++ pp reg ++ "[" ++ show offset ++ "]"
    pp (LIRJumpLabelInst label) = "JMP " ++ pp label
    pp (LIRIfInst expr label) = "IF " ++ pp expr ++ " JMP " ++ pp label
    pp (LIRCallInst call) = pp call
    pp (LIRRetOperInst operand) = "RET " ++ pp operand
    pp LIRRetInst = "RET"
    pp (LIRLabelInst label) = pp label
    treeify (LIRRegAssignInst reg expr) = Node "ASSIGN" [treeify reg, treeify expr]
    treeify (LIRRegOffAssignInst reg offset size operand) = Node "ASSIGN" [treeify reg, treeify offset, treeify size, treeify operand]
    treeify (LIRCondAssignInst reg reg' operand) = Node "CONDASSIGN" [treeify reg, treeify reg', treeify operand]
    treeify (LIRStoreInst mem operand) = Node "STR" [treeify mem, treeify operand]
    treeify (LIRLoadInst reg mem) = Node "LD" [treeify reg, treeify mem]
    treeify (LIRJumpRegInst reg offset) = Node "JMP" [treeify reg, treeify offset]
    treeify (LIRJumpLabelInst label) = Node "JMP" [treeify label]
    treeify (LIRIfInst expr label) = Node "IF" [treeify expr, treeify label]
    treeify (LIRCallInst call) = Node "CALL" [treeify call]
    treeify (LIRRetOperInst operand) = Node "RET" [treeify operand]
    treeify LIRRetInst = Node "RET" []
    treeify (LIRLabelInst label) = Node (pp label) []
    pos _     = error "LIR has no associated position"

instance IRNode LIRCall where
    pp (LIRCallAssign reg proc reg') = pp reg ++ " <- call " ++ pp proc ++ ", " ++ pp reg'
    pp (LIRCall proc reg) = "call " ++ pp proc ++ ", " ++ pp reg
    treeify (LIRCallAssign reg proc reg') = Node "CALLASSIGN" [treeify reg, treeify proc, treeify reg']
    treeify (LIRCall proc reg) = Node "CALL" [treeify proc, treeify reg]
    pos _     = error "LIR has no associated position"

instance IRNode LIRProc where
    pp (LIRProcLabel label) = label
    pp (LIRProcReg reg) = pp reg
    treeify (LIRProcLabel label) = Node label []
    treeify (LIRProcReg reg) = treeify reg
    pos _     = error "LIR has no associated position"

instance IRNode LIRExpr where
    pp (LIRBinExpr operand binop operand') = pp operand ++ " " ++ pp binop ++ " " ++ pp operand'
    pp (LIRUnExpr unop operand) = pp unop ++ pp operand
    pp (LIROperExpr operand) = pp operand
    treeify (LIRBinExpr operand binop operand') = Node (pp binop) [treeify operand, treeify operand']
    treeify (LIRUnExpr unop operand) = Node (pp unop) [treeify operand]
    treeify (LIROperExpr operand) = Node (pp operand) []
    pos _     = error "LIR has no associated position"

instance IRNode LIRRelExpr where
    pp (LIRBinRelExpr operand relop operand') = pp operand ++ " " ++ pp relop ++ " " ++ pp operand'
    pp (LIRNotRelExpr operand) = "!" ++ pp operand
    pp (LIROperRelExpr operand) = pp operand
    treeify (LIRBinRelExpr operand relop operand') = Node (pp relop) [treeify operand, treeify operand']
    treeify (LIRNotRelExpr operand) = Node "!" [treeify operand]
    treeify (LIROperRelExpr operand) = Node (pp operand) []
    pos _     = error "LIR has no associated position"

instance IRNode LIRBinOp where
    pp (LADD) = "ADD"
    pp (LMIN) = "SUB"
    pp (LMUL) = "MUL"
    pp (LDIV) = "DIV"
    pp (LMOD) = "MOD"
    pp (LAND) = "AND"
    pp (LOR) = "OR"
    pp (LXOR) = "XOR"
    pp (LSHL) = "SHL"
    pp (LSHR) = "SHR"
    pp (LSHRA) = "SHRA"
    pp (LIRBinRelOp relop) = pp relop
    treeify (LIRBinRelOp relop) = treeify relop
    treeify a = Node (pp a) []
    pos _     = error "LIR has no associated position"

instance IRNode LIRUnOp where
    pp (LNEG) = "-"
    pp (LNOT) = "!"
    treeify a = Node (pp a) []
    pos _     = error "LIR has no associated position"

instance IRNode LIRRelOp where
    pp (LNEQ) = "!="
    pp (LGT) = ">"
    pp (LGTE) = ">="
    pp (LLT) = "<"
    pp (LLTE) = "<="
    treeify a = Node (pp a) []
    pos _     = error "LIR has no associated position"

instance IRNode LIRMemAddr where
    pp (LIRRegMemAddr reg size) = "[" ++ pp reg ++ "] (" ++ pp size ++ ")"
    pp (LIRRegPlusMemAddr reg reg' size) = "[" ++ pp reg ++ " + " ++ pp reg'++ "] (" ++ pp size ++ ")"
    pp (LIRRegOffMemAddr reg offset size) = "[" ++ pp reg ++ " + " ++ pp offset++ "] (" ++ pp size ++ ")"
    treeify (LIRRegMemAddr reg size) = Node "MEM" [treeify reg, treeify size]
    treeify (LIRRegPlusMemAddr reg reg' size) = Node "MEMPLUS" [treeify reg, treeify reg', treeify size]
    treeify (LIRRegOffMemAddr reg offset size) = Node "MEMMOFF" [treeify reg, treeify offset, treeify size]
    pos _     = error "LIR has no associated position"

instance IRNode LIROperand where
    pp (LIRRegOperand reg) = pp reg
    pp (LIRIntOperand i) = pp i
    treeify a = Node (pp a) []
    pos _     = error "LIR has no associated position"

instance IRNode LIRReg where
    pp (RAX) = "RAX"
    pp (RBX) = "RBX"
    pp (RCX) = "RCX"
    pp (RDX) = "RDX"
    pp (RBP) = "RBP"
    pp (RSP) = "RSP"
    pp (RSI) = "RSI"
    pp (RDI) = "RDI"
    pp (R8) = "R8"
    pp (R9) = "R9"
    pp (R10) = "R10"
    pp (R11) = "R11"
    pp (R12) = "R12"
    pp (R13) = "R13"
    pp (R14) = "R14"
    pp (R15) = "R15"
    pp (SREG i) = "s" ++ i
    treeify a = Node (pp a) []
    pos _     = error "LIR has no associated position"

instance IRNode LIRInt where
    pp (LIRInt i) = "0x" ++ showHex i ""
    treeify i = Node (pp i) []
    pos _     = error "LIR has no associated position"

instance IRNode LIRLabel where
    pp (LIRLabel s) = s
    treeify s = Node (pp s) []
    pos _     = error "LIR has no associated position"
