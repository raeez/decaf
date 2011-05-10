{-# LANGUAGE DeriveDataTypeable
  , TypeSynonymInstances #-}

module Decaf.IR.SSA where
--import Decaf.IR.Class
import Decaf.IR.IRNode
import Decaf.Data.Tree
import Decaf.IR.SymbolTable
import Decaf.IR.LIR

import Numeric
import Data.Int
import Data.Typeable
--import Loligoptl.Label

type SSAInt    = LIRInt
type SSASize   = LIRSize
type SSAOffset = LIROffset

type SSALabel = LIRLabel

data SSAProgram = SSAProgram
    { ssaProgLabel :: SSALabel
    , ssaProgUnits :: [SSAUnit]
    } deriving (Show, Eq, Typeable)

data SSAUnit = SSAUnit
    { ssaUnitLabel ::LIRLabel
    , ssaUnitInstructions :: [SSAInst]
    } deriving (Show, Eq, Typeable)

data SSAInst = SSARegAssignInst SSAReg SSAExpr
             | SSARegOffAssignInst SSAReg SSAReg SSASize SSAOperand
             | SSAStoreInst SSAMemAddr SSAOperand
             | SSALoadInst SSAReg SSAMemAddr
             | SSAEnterInst SSAInt
             | SSAJumpLabelInst SSALabel
             | SSAIfInst SSARelExpr SSALabel SSALabel -- false, then true
             | SSACallInst SSALabel SSALabel -- method label, return label
             | SSACalloutInst String
             | SSARetInst [SSALabel] String -- list of successors, and the name of the method returning from
             | SSALabelInst SSALabel
             deriving (Show, Eq, Typeable)


data SSAProc = SSAProcLabel SSALabel
             | SSAProcReg SSAReg
             deriving (Show, Eq, Typeable)

data SSAExpr = SSABinExpr SSAOperand SSABinOp SSAOperand
             | SSAUnExpr SSAUnOp SSAOperand
             | SSAOperExpr SSAOperand
             deriving (Show, Eq, Typeable)

data SSARelExpr = SSABinRelExpr SSAOperand SSARelOp SSAOperand
                | SSANotRelExpr SSAOperand
                | SSAOperRelExpr SSAOperand
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

type SSAUnOp = LIRUnOp
type SSARelOp = LIRRelOp
type SSAMemAddr = LIRMemAddr
data SSAOperand = SSARegOperand LIRReg
                | SSAIntOperand LIRInt
                | SSAStrOperand String
                deriving (Show, Eq, Typeable, Ord)

type SSAReg = LIRReg

instance Show LIRLabel where
  show (LIRLabel s n) = if n == -1 then s else s ++ (show n)

falseLabel :: Int -> LIRLabel
falseLabel l = LIRLabel "LFalse" l

instance IRNode SSAProgram where
    pp (SSAProgram label units) = pp label ++ ":\n" ++ unlines (map pp units)
    treeify (SSAProgram label units) = Node (pp label) (map treeify units)

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
