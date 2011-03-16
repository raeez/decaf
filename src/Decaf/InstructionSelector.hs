module Decaf.InstructionSelector where
import Numeric
import Decaf.IR.SymbolTable
import Decaf.IR.LIR
import Decaf.IR.AST
import Decaf.IR.ASM

-- TODO instance ASMProgram as opposed to IRProgram
-- i.e. create a new internal representation for asm
-- along with a monadic container (dsl for superoptimization)
-- this code then just becomes the pretty printer into gnuasm, or intelasm
--

regOpen r = "mov r10, "++ intelasm r ++ sep

regSave r = "mov " ++ intelasm r ++ ", r10" ++ sep

{-case r of
              SREG s -> "mov " ++ intelasm r ++", r10" ++ sep
              GI   s -> "mov " ++ intelasm r ++", r10" ++ sep
              otherwise -> ""-}



operOpen op num = 
    "mov r1"++ show num ++", "++ intelasm op ++ sep -- selects either r10 or r11

movaddr addr op2 = (operOpen op2 1) ++ "mov "++ intelasm addr ++ ", " ++ intelasm R11

sep = "\n        "
twoop t o1@(SREG {}) o2 = (operOpen o2 1) ++ t ++ " " ++ intelasm R10 ++ ", " ++ intelasm R11 ++ sep ++ regSave o1
twoop t o1 o2 = operOpen o2 1 ++ t ++ " " ++ intelasm o1 ++ ", " ++ intelasm R11 ++ sep
mov op1 op2 = twoop "mov" op1 op2
add op1 op2 = twoop "add" op1 op2
sub op1 op2 = twoop "sub" op1 op2
cmp op1 op2 = operOpen op1 0 ++ operOpen op2 1 ++ "cmp " ++ intelasm R10 ++ ", " ++ intelasm R11
imul op = (operOpen op 0) ++ "imul " ++ intelasm R10
idiv op = (operOpen op 0) ++ "idiv " ++ intelasm R10
imod op = (operOpen op 0) ++ "idiv " ++ intelasm R10
jmp label = "jmp " ++ intelasm label
genExpr reg expr =
  case expr of
      LIRBinExpr op1' LSUB op2' -> mov R10 op1' ++ sep
                                ++ sub R10 op2' ++ sep
                                ++ regSave reg

      LIRBinExpr op1' LADD op2' -> mov R10 op1' ++ sep
                                ++ add R10 op2' ++ sep
                                ++ regSave reg

      LIRBinExpr op1' LMUL op2' -> mov RAX op1' ++ sep
                                ++ imul op2'    ++ sep
                                ++ "mov "++(intelasm reg)++ ", "++intelasm RAX

      LIRBinExpr op1' LDIV  op2' -> mov RAX op1' ++ sep
                                ++ idiv op2'     ++ sep
                                ++ "mov "++(intelasm reg)++ ", "++intelasm RAX
      LIRBinExpr op1' LMOD  op2' -> mov RAX op1' ++ sep
                                ++ idiv op2' ++ sep
                                ++ "mov "++(intelasm reg)++ ", "++intelasm RDX

      LIRBinExpr op1' (LIRBinRelOp binop label) op2' ->
            cmp op1' op2' ++ sep
         ++ jtype ++ sep
         ++ mov reg (LIRIntOperand $ LIRInt 0) ++ sep
         ++ "jmp " ++ end ++ "\n"
         ++ "    " ++ intelasm label ++ ":" ++ sep
         ++ mov reg (LIRIntOperand $ LIRInt 1) ++ "\n"
         ++ "    " ++ intelasm label ++ "_end:"
        where
          end = intelasm label ++ "_end"
          jtype = case binop of
                    LEQ -> "je " ++ intelasm label
                    LNEQ -> "jne " ++ intelasm label
                    LGT -> "jg " ++ intelasm label
                    LGTE -> "jge " ++ intelasm label
                    LLT -> "jl " ++ intelasm label
                    LLTE -> "jle " ++ intelasm label
          
      LIRBinExpr op1' (LIRBinRelOp LNEQ label) op2' -> "******************* " ++ intelasm reg ++ " <- " ++ intelasm expr
      LIRBinExpr op1' (LIRBinRelOp LGT label) op2' -> "******************* " ++ intelasm reg ++ " <- " ++ intelasm expr
      LIRBinExpr op1' (LIRBinRelOp LGTE label) op2' -> "******************* " ++ intelasm reg ++ " <- " ++ intelasm expr
      LIRBinExpr op1' (LIRBinRelOp LLT label) op2' -> "******************* " ++ intelasm reg ++ " <- " ++ intelasm expr
      LIRBinExpr op1' (LIRBinRelOp LLTE label) op2' -> "******************* " ++ intelasm reg ++ " <- " ++ intelasm expr

instance ASM SymbolTable where
    intelasm (SymbolTable records _) = "USE64\nextern printf\nextern get_int_035\nsection .data:\n" ++ unlines (indentMap records) ++ "\n"
      where
        indentMap :: [SymbolRecord] -> [String]
        indentMap [] = []
        indentMap (x:xs) = case intelasm x of
                               [] -> indentMap xs
                               _ -> ["    " ++ intelasm x] ++ indentMap xs

instance ASM SymbolRecord where
    intelasm (VarRec (DecafVar ty ident _) num) =
        "common g" ++ show (-num - 1) ++ " 8:1"

    intelasm (ArrayRec (DecafArr ty ident len _) go) =
        "common " ++ arrayLabel go ++ " " ++( show  $ 8 * (readDecafInteger len))++ ":" ++ show 8

    intelasm (StringRec str sl) =
        stringLabel sl ++ ": db " ++ show str ++ ", " ++ show 0

    intelasm _ = ""

instance ASM LIRProgram where
    intelasm (LIRProgram label units) = "section .text:\n    global main\n" ++ unlines (map intelasm units)

instance ASM LIRUnit where
    intelasm (LIRUnit label insts) =
         unitlabel label ++ unlines (indentMap insts)
      where
        unitlabel l@(LIRLabel label) = if (length label > 0) then ("\n" ++ intelasm l ++ ":\n") else ""
        indentMap [] = []
        indentMap (x:xs) = let repr = case x of
                                    LIRLabelInst (LIRLabel "") -> ""
                                    LIRLabelInst _ -> "\n    " ++ intelasm x ++ ":"
                                    _ -> "        " ++ intelasm x
                        in (repr:indentMap xs)

instance ASM LIRInst where
    intelasm (LIRRegAssignInst reg expr@(LIRBinExpr (LIRRegOperand reg') binop op2)) =
        if reg == reg'
          then case binop of
--                    LSUB -> sub reg op2

--                    LADD -> add reg op2

                    _ -> genExpr reg expr
          else genExpr reg expr

    intelasm (LIRRegAssignInst reg expr@(LIRBinExpr op1 binop op2)) =
        genExpr reg expr

    intelasm (LIRRegAssignInst reg (LIRUnExpr LNEG operand)) =
        mov reg operand ++ sep
     ++ "neg " ++ intelasm reg 

    intelasm (LIRRegAssignInst reg (LIRUnExpr LNOT operand)) =
        mov reg operand ++ sep
     ++ "not " ++ intelasm reg

    intelasm (LIRRegAssignInst reg (LIROperExpr operand)) =
        mov reg operand

    intelasm (LIRRegOffAssignInst reg offset size operand) = "******************* " ++ intelasm reg ++ "(" ++ intelasm offset ++ ", " ++ intelasm size ++ ") <- " ++ intelasm operand
    intelasm (LIRCondAssignInst reg reg' operand) = "******************* " ++ intelasm reg ++ " <- (" ++ intelasm reg' ++ ") " ++ intelasm operand

    intelasm (LIRStoreInst mem operand) =
        movaddr mem operand 

    intelasm (LIRLoadInst reg mem) =
        mov reg mem

    intelasm (LIRJumpRegInst reg offset) =
        "jmp " ++ "[" ++ intelasm reg ++ show offset ++ "]"

    intelasm (LIRJumpLabelInst label) =
        jmp label

    intelasm (LIRIfInst (LIRBinRelExpr op1 binop op2) label) =
        cmp op1 op2 ++ sep
      ++ jtype
      where
        jtype = case binop of
                    LEQ -> "je " ++ intelasm label
                    LNEQ -> "jne " ++ intelasm label
                    LGT -> "jg " ++ intelasm label
                    LGTE -> "jge " ++ intelasm label
                    LLT -> "jl " ++ intelasm label
                    LLTE -> "jle " ++ intelasm label

    intelasm (LIRIfInst (LIRNotRelExpr operand) label) =
        cmp operand (LIRIntOperand $ LIRInt 0) ++ sep
     ++ "je " ++ intelasm label

    intelasm (LIRIfInst (LIROperRelExpr operand) label) =
        cmp operand (LIRIntOperand $ LIRInt 1) ++ sep
     ++ "jge " ++ intelasm label

    intelasm (LIRCallInst proc) =
        "call " ++ intelasm proc

    intelasm (LIRTempEnterInst s) =
        "push rbp"++sep++"mov rbp, rsp"++sep++"sub rsp, "++(show $ 8*s)

    intelasm LIRRetInst =
        "mov rsp, rbp\n        pop rbp\n        ret"

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
    intelasm (LIRBinRelOp relop label) = intelasm relop

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
    intelasm (LIRRegMemAddr reg (LIRInt size)) = intelasm reg
    intelasm (LIRRegPlusMemAddr reg reg' (LIRInt size)) = intelPrefix size ++ " [" ++ intelasm reg ++ " + " ++ intelasm reg' ++ "]"
    intelasm (LIRRegOffMemAddr reg offset (LIRInt size)) = intelPrefix size ++ " [" ++ intelasm reg ++ " + " ++ intelasm offset++ "]"

instance ASM LIROperand where
    intelasm (LIRRegOperand reg) = intelasm reg
    intelasm (LIRIntOperand i) = intelasm i
    intelasm (LIRStringOperand s) = s

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
    intelasm (GI i) = "qword [g"++(show i)++"]"
    intelasm (SREG i) = "qword [rbp-" ++ show(8*(i+1))++"]"
    intelasm (MEM i) = i

instance ASM LIRInt where
    intelasm (LIRInt i) = (if i < 0 then "-" else "") ++ "0x" ++ showHex (abs i) ""

instance ASM LIRLabel where
    intelasm (LIRLabel s) = s
