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

type ASMInt = Int64

class SymbolicAssembler a where
    -- | The 'intelasm' function formats its input
    -- into a string representation for output.
    intelasm :: a -> String

    -- | The 'gnuasm' function formats its input
    -- into a string representation for output.
    gnuasm :: a -> String

data ASMProgram = ASMProgram
    { progFlags    :: Set ASMFlag
    , progExterns  :: Set ASMExternDecl
    , progSections :: [ASMSection]
    }

newtype ASMFlag = ASMFlag String
                deriving (Ord, Eq)

newtype ASMExternDecl = ASMExternDecl String
                      deriving (Ord, Eq)

data ASMSection = ASMTextSection
    { sectionInsts :: ASMList }
                | ASMDataSection
    { sectionData  :: [ASMDataDecl] }

data ASMDataDecl = ASMDataSegment ASMLabel [ASMByte]
                 | ASMCommonSegment ASMLabel ASMInt

newtype ASMByte = ASMByte Int8

data Reg
data Mem
data Lit

data ASMList where
    ASMCons :: forall t. SymbolicAssembler t =>  t ->  ASMList -> ASMList
    ASMNil :: ASMList

asmConcat :: ASMList -> ASMList -> ASMList
asmConcat ASMNil ASMNil         = ASMNil
asmConcat ASMNil (ASMCons x xs) = ASMCons x xs
asmConcat (ASMCons x xs) ASMNil = ASMCons x xs
asmConcat (ASMCons x xs) y      = ASMCons x (asmConcat xs y)

--asmMap :: (a -> b) -> ASMList -> ASMList
--asmMap f ASMNil = ASMNil
--asmMap f (ASMCons x xs) = ASMCons (f x) (asmMap f xs)

castAsmList :: forall a. SymbolicAssembler a => [a] -> ASMList
castAsmList [] = ASMNil
castAsmList (x:xs) = ASMCons x (castAsmList xs)

asmSingle :: forall a. SymbolicAssembler a => a -> ASMList
asmSingle x = ASMCons x ASMNil

--getSingle :: forall a. SymbolicAssembler a => ASMList -> a
--getSingle (ASMCons x ASMNil) = x

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
    ASMCallInst  :: ASMSym -> ASMInst ()
    ASMJmpInst   :: ASMLabel -> ASMInst ()
    ASMJeInst    :: ASMLabel -> ASMInst ()
    ASMJneInst   :: ASMLabel -> ASMInst ()
    ASMJgInst    :: ASMLabel -> ASMInst ()
    ASMJgeInst   :: ASMLabel -> ASMInst ()
    ASMJlInst    :: ASMLabel -> ASMInst ()
    ASMJleInst   :: ASMLabel -> ASMInst ()
    ASMLabelInst :: ASMLabel -> ASMInst ()
    ASMEnterInst :: Int64 -> ASMInst ()
    ASMRetInst   :: ASMInst ()

data ASMMemBase = ASMRegBase ASMReg
                | ASMSymBase ASMSym

data ASMOperand a where
    ASMMemOperand :: ASMMemBase -> Maybe ASMMemBase -> Int64 -> Int64 -> ASMOperand Mem
    ASMSymOperand :: ASMSym -> Int64 -> Int64 -> ASMOperand Mem
    ASMPntOperand :: ASMSym -> ASMOperand Mem
    ASMRegOperand :: ASMReg -> Int64 -> ASMOperand Reg
    ASMLitOperand :: Int64 -> ASMOperand Lit

data ASMGenOperand where
    ASMGenOperand :: forall t. ASMOperand t -> ASMGenOperand

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
-- x86 instructions.
gnuSuffix :: Int64 -> Char
gnuSuffix size = case size of
                      1 -> 'b'
                      2 -> 'w'
                      4 -> 'd'
                      8 -> 'q'

-- | 'intelPrefix' calculates the word-size delimiter for
-- memory addressing modes.
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

sep :: String
sep = "\n        "

instance SymbolicAssembler ASMProgram where
    intelasm (ASMProgram flags externs sections) =
        unlines (map intelasm $ elems flags)
     ++ unlines (map intelasm $ elems externs)
     ++ unlines (indentLabels intelasm sections)

instance SymbolicAssembler ASMFlag where
    intelasm (ASMFlag flag) = flag

instance SymbolicAssembler ASMExternDecl where
    intelasm (ASMExternDecl extern) = "extern " ++ extern

-- | the following instance declaration implements the following:
-- (unlines . reverse) map intelasm (insts :: [forall a. ASMInst a])
instance SymbolicAssembler ASMList where
    intelasm (ASMCons x xs) = intelasm xs ++ "\n" ++  intelasm x 
    intelasm (ASMNil) = ""

instance SymbolicAssembler ASMSection where
    intelasm (ASMDataSection decls) =
        "section .data\n"
     ++ unlines (indentLabels intelasm decls)

    intelasm (ASMTextSection text) =
        "section .text:\n" ++ indent
     ++ "global main\n"
     ++ (unlines . indentMap . lines . intelasm) text

instance SymbolicAssembler ASMDataDecl where
    intelasm (ASMDataSegment label bytes) =
        intelasm label ++ ": db " ++ outputBytes
      where
        outputBytes = concat $ intersperse ", " (map intelasm bytes)

    intelasm (ASMCommonSegment label size) =
        "common " ++ intelasm label ++ " " ++ show (size * 8) ++ ":8"

instance SymbolicAssembler ASMByte where
    intelasm (ASMByte byte) =
        (if byte < 0 then "-" else "") ++ "0x" ++ showHex (abs byte) ""
 

instance SymbolicAssembler ASMGenOperand where
    intelasm (ASMGenOperand o) = intelasm o

instance SymbolicAssembler ASMMemBase where
    intelasm (ASMRegBase reg) = intelasm reg
    intelasm (ASMSymBase sym) = intelasm sym

instance forall a. SymbolicAssembler (ASMInst a) where
    intelasm (ASMMovInst op1 op2) =
        "mov " ++ intelasm op1 ++ ", " ++ intelasm op2

    intelasm (ASMAddInst op1 op2) =
        "add "  ++ intelasm op1 ++ ", " ++ intelasm op2

    intelasm (ASMSubInst op1 op2) =
        "sub " ++ intelasm op1 ++ ", " ++ intelasm op2

    intelasm (ASMCmpInst op1 op2) =
        "cmp " ++ intelasm op1 ++ ", " ++ intelasm op2

    --intelasm (ASMAndInst ASMOperand a -> ASMOperand b -> ASMInst (a, b)) =
    --intelasm (ASMOrInst  ASMOperand a -> ASMOperand b -> ASMInst (a, b)) =
    --intelasm (ASMXorInst ASMOperand a -> ASMOperand b -> ASMInst (a, b)) =
    --intelasm (ASMShlInst ASMOperand a -> ASMOperand b -> ASMInst (a, b)) =
    --intelasm (ASMShrInst ASMOperand a -> ASMOperand b -> ASMInst (a, b)) =
    --intelasm (ASMShraInstASMOperand a -> ASMOperand b -> ASMInst (a, b)) =
 
    intelasm (ASMMulInst op) =
        "imul " ++ intelasm op

    intelasm (ASMDivInst op) =
        "div " ++ intelasm op

    intelasm (ASMModInst op) =
        "mod " ++ intelasm op

    intelasm (ASMPushInst op) =
        "push " ++ intelasm op

    intelasm (ASMPopInst op) = 
        "pop " ++ intelasm op

    intelasm (ASMCallInst proc) =
        "call " ++ intelasm proc

    intelasm (ASMJmpInst label) =
        "jmp " ++ intelasm label

    intelasm (ASMJeInst label) =
        "je " ++ intelasm label

    intelasm (ASMJneInst label) =
        "jne " ++ intelasm label

    intelasm (ASMJgInst label) =
        "jg " ++ intelasm label

    intelasm (ASMJgeInst label) =
        "jge " ++ intelasm label

    intelasm (ASMJlInst label) =
        "jl " ++ intelasm label

    intelasm (ASMJleInst label) =
        "jle " ++ intelasm label

    intelasm (ASMEnterInst offset) =
        "enter " ++ literalDisplay offset 

    intelasm (ASMNegInst op) =
        "neg " ++ intelasm op

    intelasm (ASMNotInst op) =
        "not " ++ intelasm op

    intelasm (ASMLabelInst label) =
        intelasm label ++ ":"

    intelasm (ASMRetInst) =
        "ret"

intelMemAddr :: SymbolicAssembler a => a -> Maybe a -> Int64 -> Int64 -> String
intelMemAddr base reg offset size = 
    intelPrefix size ++ " [" ++ intelasm base ++ b' ++ off ++ "]"
  where
      off
        | offset /= 0 = literalDisplay offset
        | otherwise  = ""

      b' = case reg of
              Just r -> " + " ++ intelasm r
              Nothing  -> ""

literalDisplay :: Int64 -> String
literalDisplay literal =
    (if literal < 0 then "-" else "") ++ "0x" ++ showHex (abs literal) ""

instance SymbolicAssembler (ASMOperand a) where
    intelasm (ASMRegOperand reg size) =
        intelasm reg

    intelasm (ASMSymOperand base offset size) =
        intelMemAddr base Nothing offset size

    intelasm (ASMPntOperand base) =
        intelasm base

    intelasm (ASMLitOperand l) =
        literalDisplay l

    intelasm (ASMMemOperand base reg offset size) =
        intelMemAddr base reg offset size

instance SymbolicAssembler ASMReg where
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
    gnuasm (RAX) = "%rax"
    gnuasm (RBX) = "%rbx"
    gnuasm (RCX) = "%rcx"
    gnuasm (RDX) = "%rdx"
    gnuasm (RBP) = "%rbp"
    gnuasm (RSP) = "%rsp"
    gnuasm (RSI) = "%rsi"
    gnuasm (RDI) = "%rdi"
    gnuasm (R8) = "%r8"
    gnuasm (R9) = "%r9"
    gnuasm (R10) = "%r10"
    gnuasm (R11) = "%r11"
    gnuasm (R12) = "%r12"
    gnuasm (R13) = "%r13"
    gnuasm (R14) = "%r14"
    gnuasm (R15) = "%r15"

instance SymbolicAssembler ASMSym where
    intelasm (ASMSym sym) = sym

instance SymbolicAssembler ASMLabel where
    intelasm (ASMLabel label) = label
