module Decaf.InstructionSelector where
import Numeric
import Decaf.IR.SymbolTable
import Decaf.IR.LIR
import Decaf.IR.ASM

instance ASM  LIRProgram where
    intelasm (LIRProgram label units) = intelasm label ++ ":\n" ++ unlines (map intelasm units)

instance ASM LIRUnit where
    intelasm (LIRUnit label insts) =
        "\n" ++ intelasm label ++ ":\n" ++ unlines (indentMap insts)
      where
        indentMap [] = []
        indentMap (x:xs) = let repr = case x of
                                    LIRLabelInst _ -> "\n" ++ intelasm x ++ ":"
                                    _ -> "    " ++ intelasm x
                        in (repr:indentMap xs)

instance ASM LIRInst where
    intelasm (LIRRegAssignInst reg expr) = intelasm reg ++ " <- " ++ intelasm expr
    intelasm (LIRRegOffAssignInst reg offset size operand) = intelasm reg ++ "(" ++ intelasm offset ++ ", " ++ intelasm size ++ ") <- " ++ intelasm operand
    intelasm (LIRCondAssignInst reg reg' operand) = intelasm reg ++ " <- (" ++ intelasm reg' ++ ") " ++ intelasm operand
    intelasm (LIRStoreInst mem operand) = "STORE " ++ intelasm mem ++ ", " ++ intelasm operand
    intelasm (LIRLoadInst reg mem) = "LOAD " ++ intelasm reg ++ ", " ++ intelasm mem
    intelasm (LIRTempLoadInst reg mem) = "TEMPLOAD " ++ intelasm reg ++ ",  " ++ intelasm mem
    intelasm (LIRJumpRegInst reg offset) = "JMP " ++ intelasm reg ++ "[" ++ show offset ++ "]"
    intelasm (LIRJumpLabelInst label) = "JMP " ++ intelasm label
    intelasm (LIRIfInst expr label) = "IF " ++ intelasm expr ++ " JMP " ++ intelasm label
    intelasm (LIRCallInst proc reg) = "call " ++ intelasm proc ++ ", " ++ intelasm reg
    intelasm (LIRRetOperInst operand) = "RET " ++ intelasm operand
    intelasm LIRRetInst = "RET"
    intelasm (LIRLabelInst label) = intelasm label

instance ASM LIRProc where
    intelasm (LIRProcLabel label) = label
    intelasm (LIRProcReg reg) = intelasm reg

instance ASM LIRExpr where
    intelasm (LIRBinExpr operand binop operand') = intelasm operand ++ " " ++ intelasm binop ++ " " ++ intelasm operand'
    intelasm (LIRUnExpr unop operand) = intelasm unop ++ intelasm operand
    intelasm (LIROperExpr operand) = intelasm operand

instance ASM LIRRelExpr where
    intelasm (LIRBinRelExpr operand relop operand') = intelasm operand ++ " " ++ intelasm relop ++ " " ++ intelasm operand'
    intelasm (LIRNotRelExpr operand) = "!" ++ intelasm operand
    intelasm (LIROperRelExpr operand) = intelasm operand

instance ASM LIRBinOp where
    intelasm (LADD) = "ADD"
    intelasm (LSUB) = "SUB"
    intelasm (LMUL) = "MUL"
    intelasm (LDIV) = "DIV"
    intelasm (LMOD) = "MOD"
    intelasm (LAND) = "AND"
    intelasm (LOR) = "OR"
    intelasm (LXOR) = "XOR"
    intelasm (LSHL) = "SHL"
    intelasm (LSHR) = "SHR"
    intelasm (LSHRA) = "SHRA"
    intelasm (LIRBinRelOp relop) = intelasm relop

instance ASM LIRUnOp where
    intelasm (LNEG) = "-"
    intelasm (LNOT) = "!"

instance ASM LIRRelOp where
    intelasm (LEQ) = "=="
    intelasm (LNEQ) = "!="
    intelasm (LGT) = ">"
    intelasm (LGTE) = ">="
    intelasm (LLT) = "<"
    intelasm (LLTE) = "<="

instance ASM LIRMemAddr where
    intelasm (LIRRegMemAddr reg (LIRInt size)) = intelPrefix size ++ " [" ++ intelasm reg ++ "]"
    intelasm (LIRRegPlusMemAddr reg reg' size) = "[" ++ intelasm reg ++ " + " ++ intelasm reg'++ "] (" ++ intelasm size ++ ")"
    intelasm (LIRRegOffMemAddr reg offset size) = "[" ++ intelasm reg ++ " + " ++ intelasm offset++ "] (" ++ intelasm size ++ ")"

instance ASM LIROperand where
    intelasm (LIRRegOperand reg) = intelasm reg
    intelasm (LIRIntOperand i) = intelasm i
    intelasm (LIRStringOperand s) = show s

instance ASM LIRReg where
    intelasm (RAX) = "rax"
    intelasm (RBX) = "rbx"
    intelasm (RCX) = "rcx"
    intelasm (RDX) = "rdx"
    intelasm (RBP) = "rbp"
    intelasm (RSP) = "rsp"
    intelasm (RSI) = "rsi"
    intelasm (RDI) = "rdi"
    intelasm (R8) = "r8"
    intelasm (R9) = "r9"
    intelasm (R10) = "r10"
    intelasm (R11) = "r11"
    intelasm (R12) = "r12"
    intelasm (R13) = "r13"
    intelasm (R14) = "r14"
    intelasm (R15) = "r15"
    intelasm (GP)  = "GP"
    intelasm (IP)  = "IP"
    intelasm (SREG i) = "s" ++ (show i)

instance ASM LIRInt where
    intelasm (LIRInt i) = "0x" ++ (if i < 0 then "-" else "") ++ showHex (abs i) ""

instance ASM LIRLabel where
    intelasm (LIRLabel s) = s
