{-# LANGUAGE ExplicitForAll 
  , RankNTypes
  , GADTs
  , EmptyDataDecls
  , ScopedTypeVariables #-}

module Decaf.IR.ASM where
import Numeric
import Data.List
import Data.Char
import Data.Int
import Data.Set hiding (map)

-- |'ASMInt' represents a bounded literal in the target language.
-- we use 'Int64' in order to enforce (host-)compile-time garuntees
-- about the size of data inside the assembler.
type ASMInt = Int64

-- |'ASMByte' represents a single byte in the target language.
-- we use a newtype on 'Int8' in order to i) enforce (host-)compile-time
-- garuntees about the size of individual bytes and ii) in order to display
-- individual bytes in context-specific settings (typically as an instance of
-- the SymbolicAssembler class).
newtype ASMByte = ASMByte Int8

-- |The 'SymbolicAssembler' class specifies the suite of objects
-- that symbolically represent assembled opcodes and operands. The
-- '___asm' functions specify conversion of the symbolic representation
-- into a style-specific string representation, commonly understood by
-- various external assemblers or compilers.
class SymbolicAssembler a where
    -- |The 'nasm' function formats its input
    -- into a string representation understood
    -- by the NASM family of assemblers.
    nasm :: a -> String

    -- |The 'gnuasm' function formats its input
    -- into a string representation understood
    -- by gcc.
    gnuasm :: a -> String

-- |Represents a syntatically correct (by guarunteed statically) 
-- x86_64 assembly program, ready to be passed to an assembler.
data ASMProgram = ASMProgram
    { progFlags    :: Set ASMFlag
    , progExterns  :: Set ASMExternDecl
    , progSections :: [ASMSection]
    }

-- |Represents a general assembler flag; usually assembler-specific.
newtype ASMFlag = ASMFlag String
                deriving (Ord, Eq)

-- |Represents a declaration of the presence of a non-local symbol
-- (requiring external linkage) in the current program.
newtype ASMExternDecl = ASMExternDecl String
                      deriving (Ord, Eq)

-- |Represents a segment of an assembly program; currently both .text and .data
-- segments are required in order to output any assembler-specific assembly code.
data ASMSection = ASMTextSection { sectionInsts :: ASMInstructions }
                | ASMDataSection { sectionData  :: [ASMDataDecl] }

-- |Represents a static declaration of global data, available to the program
-- as pointers to memory regions represented symbolically.
data ASMDataDecl = ASMDataSegment ASMLabel [ASMByte]
                 | ASMCommonSegment ASMLabel ASMInt

-- |Phantom-type indicator of an ASMOperand that is a register.
data Reg

-- |Phantom-type indicator of an ASMOperand that is a pointer to a memory region.
data Mem

-- |Phantom-type indicator of an ASMOperand that is a static integer literal.
data Lit

data ASMGenOperand where
    ASMGenOperand :: forall t. ASMOperand t -> ASMGenOperand

data ASMOperand a where
    ASMMemOperand :: ASMMemBase -> Maybe ASMMemBase -> Int64 -> Int64 -> ASMOperand Mem
    ASMSymOperand :: ASMSym -> ASMOperand Mem
    ASMRegOperand :: ASMReg -> Int64 -> ASMOperand Reg
    ASMLitOperand :: Int64 -> ASMOperand Lit

-- |Represents a single opcode (one, two or three bytes) in the x86_64
-- instruction set. ASMInst is a GADT parameterized 
data ASMInst a where
    ASMAddInst   :: ASMOperand a -> ASMOperand b -> ASMInst (a, b)
    ASMSubInst   :: ASMOperand a -> ASMOperand b -> ASMInst (a, b)
    ASMMovInst   :: ASMOperand a -> ASMOperand b -> ASMInst (a, b)
    ASMCmpInst   :: ASMOperand a -> ASMOperand b -> ASMInst (a, b)
    ASMAndInst   :: ASMOperand a -> ASMOperand b -> ASMInst (a, b)
    ASMOrInst    :: ASMOperand a -> ASMOperand b -> ASMInst (a, b)
    ASMXorInst   :: ASMOperand a -> ASMOperand b -> ASMInst (a, b)
    ASMShlInst   :: ASMOperand a -> ASMOperand b -> ASMInst (a, b)
    ASMShrInst   :: ASMOperand a -> ASMOperand b -> ASMInst (a, b)
    ASMShraInst  :: ASMOperand a -> ASMOperand b -> ASMInst (a, b)
    ASMMulInst   :: ASMOperand a -> ASMInst a
    ASMDivInst   :: ASMOperand a -> ASMInst a
    ASMModInst   :: ASMOperand a -> ASMInst a
    ASMPushInst  :: ASMOperand a -> ASMInst a
    ASMPopInst   :: ASMOperand a -> ASMInst a
    ASMNegInst   :: ASMOperand a -> ASMInst a
    ASMNotInst   :: ASMOperand a -> ASMInst a
    ASMJmpInst   :: ASMLabel -> ASMInst ()
    ASMJeInst    :: ASMLabel -> ASMInst ()
    ASMJneInst   :: ASMLabel -> ASMInst ()
    ASMJgInst    :: ASMLabel -> ASMInst ()
    ASMJgeInst   :: ASMLabel -> ASMInst ()
    ASMJlInst    :: ASMLabel -> ASMInst ()
    ASMJleInst   :: ASMLabel -> ASMInst ()
    ASMLabelInst :: ASMLabel -> ASMInst ()
    ASMCallInst  :: ASMSym -> ASMInst ()
    ASMEnterInst :: ASMInt -> ASMInst ()
    ASMRetInst   :: ASMInst ()

data ASMInstructions where
    ASMCons :: forall t. SymbolicAssembler t =>  t ->  ASMInstructions -> ASMInstructions
    ASMNil :: ASMInstructions

asmConcat :: ASMInstructions -> ASMInstructions -> ASMInstructions
asmConcat ASMNil ASMNil         = ASMNil
asmConcat ASMNil (ASMCons x xs) = ASMCons x xs
asmConcat (ASMCons x xs) ASMNil = ASMCons x xs
asmConcat (ASMCons x xs) y      = ASMCons x (asmConcat xs y)

castAsmList :: forall a. SymbolicAssembler a => [a] -> ASMInstructions
castAsmList [] = ASMNil
castAsmList (x:xs) = ASMCons x (castAsmList xs)

data ASMMemBase = ASMRegBase ASMReg
                | ASMSymBase ASMSym

data ASMReg = RAX
            | RCX
            | RDX
            | RBX
            | RSP
            | RBP
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

lit :: Int64 -> ASMGenOperand
lit i = ASMGenOperand $ ASMLitOperand i

rax, rbx, rcx, rdx, rbp, rsp, rdi, r8, r9, r10, r11, r12, r13, r14, r15 ::
   ASMGenOperand

rax = ASMGenOperand $ ASMRegOperand RAX 8
rbx = ASMGenOperand $ ASMRegOperand RBX 8
rcx = ASMGenOperand $ ASMRegOperand RCX 8
rdx = ASMGenOperand $ ASMRegOperand RDX 8
rbp = ASMGenOperand $ ASMRegOperand RBP 8
rsp = ASMGenOperand $ ASMRegOperand RSP 8
rsi = ASMGenOperand $ ASMRegOperand RSI 8
rdi = ASMGenOperand $ ASMRegOperand RDI 8
r8  = ASMGenOperand $ ASMRegOperand R8 8
r9  = ASMGenOperand $ ASMRegOperand R9 8
r10 = ASMGenOperand $ ASMRegOperand R10 8
r11 = ASMGenOperand $ ASMRegOperand R11 8
r12 = ASMGenOperand $ ASMRegOperand R12 8
r13 = ASMGenOperand $ ASMRegOperand R13 8
r14 = ASMGenOperand $ ASMRegOperand R14 8
r15 = ASMGenOperand $ ASMRegOperand R15 8

newtype ASMSym = ASMSym String

newtype ASMLabel = ASMLabel String

-- | 'gnuSuffix' calculates the opcode suffix for
-- x86_64 instructions.
gnuSuffix :: Int64 -> Char
gnuSuffix size = case size of
                      1 -> 'b'
                      2 -> 'w'
                      4 -> 'd'
                      8 -> 'q'

-- | 'intelPrefix' calculates the word-size delimiter for
-- memory addressing modes, placed before the square brackets.
intelPrefix :: Int64 -> String
intelPrefix size = case size of
                      0 -> "" -- undeclared
                      1 -> "byte"
                      2 -> "word"
                      4 -> "dword"
                      8 -> "qword"

indentLabels :: forall a. SymbolicAssembler a => (a -> String) -> [a] -> [String]
indentLabels f [] = []
indentLabels f (x:xs) = case f x of
    [] -> indentLabels f xs
    _  -> ["    " ++ f x] ++ indentLabels f xs

indentMap :: [String] -> [String]
indentMap [] = []
indentMap (x:xs) = case x of
    [] -> indentMap xs
    _  -> if (head . reverse) x == ':'
            then ["\n"] ++ ["    " ++ x] ++ indentMap xs
            else ["        " ++ x] ++ indentMap xs

indent :: String
indent = "    "

instance SymbolicAssembler ASMProgram where
    nasm (ASMProgram flags externs sections) =
        unlines (map nasm $ elems flags)
     ++ unlines (map nasm $ elems externs)
     ++ unlines (indentLabels nasm sections)

instance SymbolicAssembler ASMFlag where
    nasm (ASMFlag flag) = flag

instance SymbolicAssembler ASMExternDecl where
    nasm (ASMExternDecl extern) = "extern " ++ extern

-- | the following instance declaration implements the following:
-- (unlines . reverse) map nasm (insts :: [forall a. ASMInst a])
instance SymbolicAssembler ASMInstructions where
    nasm (ASMCons x xs) = nasm xs ++ "\n" ++  nasm x 
    nasm (ASMNil) = ""

instance SymbolicAssembler ASMSection where
    nasm (ASMDataSection decls) =
        "section .data:\n"
     ++ unlines (indentLabels nasm decls)

    nasm (ASMTextSection text) =
        "section .text:\n" ++ indent
     ++ "global main\n"
     ++ (unlines . indentMap . lines . nasm) text

instance SymbolicAssembler ASMDataDecl where
    nasm (ASMDataSegment label bytes) =
        nasm label ++ ": db " ++ outputBytes
      where
        outputBytes = concat $ intersperse ", " (map nasm bytes)

    nasm (ASMCommonSegment label size) =
        "common " ++ nasm label ++ " " ++ show (size * 8) ++ ":8"

instance SymbolicAssembler ASMByte where
    nasm (ASMByte byte) =
        (if byte < 0 then "-" else "") ++ "0x" ++ showHex (abs byte) ""

instance SymbolicAssembler ASMGenOperand where
    nasm (ASMGenOperand o) = nasm o

instance SymbolicAssembler ASMMemBase where
    nasm (ASMRegBase reg) = nasm reg
    nasm (ASMSymBase sym) = nasm sym

instance forall a. SymbolicAssembler (ASMInst a) where
    nasm (ASMMovInst op1 op2) =
        "mov " ++ nasm op1 ++ ", " ++ nasm op2

    nasm (ASMAddInst op1 op2) =
        "add "  ++ nasm op1 ++ ", " ++ nasm op2

    nasm (ASMSubInst op1 op2) =
        "sub " ++ nasm op1 ++ ", " ++ nasm op2

    nasm (ASMCmpInst op1 op2) =
        "cmp " ++ nasm op1 ++ ", " ++ nasm op2

    --nasm (ASMAndInst ASMOperand a -> ASMOperand b -> ASMInst (a, b)) =
    --nasm (ASMOrInst  ASMOperand a -> ASMOperand b -> ASMInst (a, b)) =
    --nasm (ASMXorInst ASMOperand a -> ASMOperand b -> ASMInst (a, b)) =
    --nasm (ASMShlInst ASMOperand a -> ASMOperand b -> ASMInst (a, b)) =
    --nasm (ASMShrInst ASMOperand a -> ASMOperand b -> ASMInst (a, b)) =
    --nasm (ASMShraInstASMOperand a -> ASMOperand b -> ASMInst (a, b)) =
 
    nasm (ASMMulInst op) =
        "imul " ++ nasm op

    nasm (ASMDivInst op) =
        "div " ++ nasm op

    nasm (ASMModInst op) =
        "mod " ++ nasm op

    nasm (ASMPushInst op) =
        "push " ++ nasm op

    nasm (ASMPopInst op) = 
        "pop " ++ nasm op

    nasm (ASMCallInst proc) =
        "call " ++ nasm proc

    nasm (ASMJmpInst label) =
        "jmp " ++ nasm label

    nasm (ASMJeInst label) =
        "je " ++ nasm label

    nasm (ASMJneInst label) =
        "jne " ++ nasm label

    nasm (ASMJgInst label) =
        "jg " ++ nasm label

    nasm (ASMJgeInst label) =
        "jge " ++ nasm label

    nasm (ASMJlInst label) =
        "jl " ++ nasm label

    nasm (ASMJleInst label) =
        "jle " ++ nasm label

    nasm (ASMEnterInst offset) =
        "enter " ++ literalDisplay offset 

    nasm (ASMNegInst op) =
        "neg " ++ nasm op

    nasm (ASMNotInst op) =
        "not " ++ nasm op

    nasm (ASMLabelInst label) =
        nasm label ++ ":"

    nasm (ASMRetInst) =
        "ret"

literalDisplay :: ASMInt -> String
literalDisplay literal =
    (if literal < 0 then "-" else "") ++ "0x" ++ showHex (abs literal) ""

instance SymbolicAssembler (ASMOperand a) where
    nasm (ASMRegOperand reg size) =
        nasm reg

    nasm (ASMSymOperand sym) =
        nasm sym

    nasm (ASMLitOperand l) =
        literalDisplay l

    nasm (ASMMemOperand base reg offset size) =
        intelPrefix size ++ " [" ++ nasm base ++ reg' ++ off ++ "]"
      where
        off
          | offset /= 0 = "+" ++ literalDisplay offset
          | otherwise  = ""

        reg' = case reg of
                  Just r -> "+" ++ nasm r
                  Nothing  -> ""

instance SymbolicAssembler ASMReg where
    nasm (RAX) = "rax"
    nasm (RBX) = "rbx"
    nasm (RCX) = "rcx"
    nasm (RDX) = "rdx"
    nasm (RBP) = "rbp"
    nasm (RSP) = "rsp"
    nasm (RSI) = "rsi"
    nasm (RDI) = "rdi"
    nasm (R8)  = "r8"
    nasm (R9)  = "r9"
    nasm (R10) = "r10"
    nasm (R11) = "r11"
    nasm (R12) = "r12"
    nasm (R13) = "r13"
    nasm (R14) = "r14"
    nasm (R15)  = "r15"
    gnuasm (RAX) = "%rax"
    gnuasm (RBX) = "%rbx"
    gnuasm (RCX) = "%rcx"
    gnuasm (RDX) = "%rdx"
    gnuasm (RBP) = "%rbp"
    gnuasm (RSP) = "%rsp"
    gnuasm (RSI) = "%rsi"
    gnuasm (RDI) = "%rdi"
    gnuasm (R8)  = "%r8"
    gnuasm (R9)  = "%r9"
    gnuasm (R10) = "%r10"
    gnuasm (R11) = "%r11"
    gnuasm (R12) = "%r12"
    gnuasm (R13) = "%r13"
    gnuasm (R14) = "%r14"
    gnuasm (R15) = "%r15"

instance SymbolicAssembler ASMSym where
    nasm (ASMSym sym) = sym

instance SymbolicAssembler ASMLabel where
    nasm (ASMLabel label) = label
