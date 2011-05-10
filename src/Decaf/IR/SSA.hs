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

data LIRBinOp = SADD
              | SSUB
              | SMUL
              | SDIV
              | SMOD
              | SAND
              | SOR
              | SXOR
              | SSHL
              | SSHR
              | SSHRA
              | SSABinRelOp SSARelOp SSALabel
              deriving (Show, Eq, Typeable, Ord)

data SSAUnOp = SNEG
             | SNOT
             deriving (Show, Eq, Typeable)

data SSARelOp = SEQ
              | SNEQ
              | SGT
              | SGTE
              | SLT
              | SLTE
              deriving (Show, Eq, Typeable, Ord)

data SSAMemAddr = SSAMemAddr SSAReg (Maybe SSAReg) SSAOffset SSASize
                deriving (Show, Eq, Typeable)

data SSAOperand = SSARegOperand SSAReg
                | SSAIntOperand SSAInt
                | SSAStrOperand String
                deriving (Show, Eq, Typeable, Ord)

data SSAReg = SRAX Int -- Int is the subscript, starting at 0
            | SRBX Int
            | SRCX Int
            | SRDX Int
            | SRBP Int
            | SRSP Int
            | SRSI Int
            | SRDI Int
            | SR8 Int
            | SR9 Int
            | SR10 Int
            | SR11 Int
            | SR12 Int
            | SR13 Int
            | SR14 Int
            | SR15 Int
            | GI Int Int
            | SREG Int Int
            | MEM String Int
            deriving (Show, Eq, Typeable, Ord)

instance IRNode SSAProgram where
    pp (SSAProgram label units) = pp label ++ ":\n" ++ unlines (map pp units)
    treeify (SSAProgram label units) = Node (pp label) (map treeify units)

instance IRNode SSAUnit where
    pp (SSAUnit label insts) =
        "\n" ++ pp label ++ ":\n" ++ unlines (indentMap insts)
      where
        indentMap [] = []
        indentMap (x:xs) = let repr = case x of
                                    SSALabelInst _ -> "\n" ++ pp x ++ ":"
                                    _ -> "    " ++ pp x
                        in (repr:indentMap xs)
    treeify (SSAUnit label insts) = Node ("SSAUnit: " ++ pp label) (map treeify insts)

instance IRNode SSAInst where
    pp (SSARegAssignInst reg expr) = pp reg ++ " <- " ++ pp expr
    pp (SSARegOffAssignInst reg offset size operand) =
        pp reg ++ "(" ++ pp offset ++ ", " ++ pp size ++ ") <- " ++ pp operand
    pp (SSAStoreInst mem operand) = "STORE " ++ pp mem ++ ", " ++ pp operand
    pp (SSALoadInst reg mem) = "LOAD " ++ pp reg ++ ", " ++ pp mem
    pp (SSAEnterInst num) = "ENTER " ++ (show num)
    pp (SSAJumpLabelInst label) = "JMP " ++ pp label
    pp (SSAIfInst expr flab tlab) = "IF " ++ pp expr ++ " THEN " ++ pp tlab ++ " ELSE " ++ pp flab
    pp (SSACallInst proc ret) = "CALL " ++ pp proc ++ " AND RETURN TO " ++ pp ret
    pp (SSACalloutInst proc) = "CALL " ++ proc
    pp (SSARetInst _ _) = "RET"
    pp (SSALabelInst label) = "LABEL: " ++ pp label
    treeify (SSARegAssignInst reg expr) =
        Node "ASSIGN" [treeify reg, treeify expr]
    treeify (SSARegOffAssignInst reg offset size operand) =
        Node "ASSIGN" $ [treeify reg, treeify offset, treeify size, treeify operand]
    treeify (SSAStoreInst mem operand) =
        Node "STR" [treeify mem, treeify operand]
    treeify (SSALoadInst reg mem) = Node "LD" [treeify reg, treeify mem]
    treeify (SSAEnterInst bytes) = Node ("Enter[" ++ pp bytes ++ "]") []
    treeify (SSAJumpLabelInst label) = Node "JMP" [treeify label]
    treeify (SSAIfInst expr flab tlab) = Node "IF" [treeify expr, treeify flab, treeify tlab]
    treeify (SSACallInst proc ret) = Node "CALL" [treeify proc]
    treeify (SSARetInst _ _) = Node "RET" []
    treeify (SSALabelInst label) = Node (pp label) []

instance IRNode SSAProc where
    pp (SSAProcLabel label) = pp label
    pp (SSAProcReg reg) = pp reg
    treeify (SSAProcLabel label) = Node (pp label) []
    treeify (SSAProcReg reg) = treeify reg

instance IRNode SSAExpr where
    pp (SSABinExpr operand binop operand') =
        pp operand ++ " " ++ pp binop ++ " " ++ pp operand'
    pp (SSAUnExpr unop operand) = pp unop ++ pp operand
    pp (SSAOperExpr operand) = pp operand
    treeify (SSABinExpr operand binop operand') =
        Node (pp binop) [treeify operand, treeify operand']
    treeify (SSAUnExpr unop operand) = Node (pp unop) [treeify operand]
    treeify (SSAOperExpr operand) = Node (pp operand) []

instance IRNode SSARelExpr where
    pp (SSABinRelExpr operand relop operand') =
        pp operand ++ " " ++ pp relop ++ " " ++ pp operand'
    pp (SSANotRelExpr operand) = "!" ++ pp operand
    pp (SSAOperRelExpr operand) = pp operand
    treeify (SSABinRelExpr operand relop operand') =
        Node (pp relop) [treeify operand, treeify operand']
    treeify (SSANotRelExpr operand) = Node "!" [treeify operand]
    treeify (SSAOperRelExpr operand) = Node (pp operand) []

instance IRNode SSABinOp where
    pp (SADD) = "ADD"
    pp (SSUB) = "SUB"
    pp (SMUL) = "MUL"
    pp (SDIV) = "DIV"
    pp (SMOD) = "MOD"
    pp (SAND) = "AND"
    pp (SOR) = "OR"
    pp (SXOR) = "XOR"
    pp (SSHL) = "SHL"
    pp (SSHR) = "SHR"
    pp (SSHRA) = "SHRA"
    pp (SSABinRelOp relop _) = pp relop
    treeify (SSABinRelOp relop _) = treeify relop
    treeify a = Node (pp a) []

instance IRNode SSAUnOp where
    pp (SNEG) = "-"
    pp (SNOT) = "!"
    treeify a = Node (pp a) []

instance IRNode SSARelOp where
    pp (SEQ) = "=="
    pp (SNEQ) = "!="
    pp (SGT) = ">"
    pp (SGTE) = ">="
    pp (SLT) = "<"
    pp (SLTE) = "<="
    treeify a = Node (pp a) []

instance IRNode SSAMemAddr where
    pp (SSAMemAddr base reg offset _) =
        "[" ++ pp base ++ " + " ++ reg' ++ " + " ++ pp offset ++ "]"
      where
        reg' = case reg of
                  Just r -> pp r
                  Nothing -> ""

    treeify (SSAMemAddr base reg offset size) =
        Node "MemAddr" $ [treeify base] ++ reg' ++ [treeify offset, treeify size]
      where
        reg' = case reg of
                  Just r -> [treeify r]
                  Nothing -> []

instance IRNode SSAOperand where
    pp (SSARegOperand reg) = pp reg
    pp (SSAIntOperand i) = pp i
    pp (SSAStrOperand s) = s
    treeify a = Node (pp a) []

instance IRNode SSAReg where
    pp (LRAX s) = "RAX[" ++ show s ++ "]"
    pp (LRBX s) = "RBX" ++ "[" ++ show s ++ "]"
    pp (LRCX s) = "RCX" ++ "[" ++ show s ++ "]"
    pp (LRDX s) = "RDX" ++ "[" ++ show s ++ "]"
    pp (LRBP s) = "RBP" ++ "[" ++ show s ++ "]"
    pp (LRSP s) = "RSP" ++ "[" ++ show s ++ "]"
    pp (LRSI s) = "RSI" ++ "[" ++ show s ++ "]"
    pp (LRDI s) = "RDI" ++ "[" ++ show s ++ "]"
    pp (LR8 s)  = "R8" ++ "[" ++ show s ++ "]"
    pp (LR9 s)  = "R9" ++ "[" ++ show s ++ "]"
    pp (LR10 s) = "R10" ++ "[" ++ show s ++ "]"
    pp (LR11 s) = "R11" ++ "[" ++ show s ++ "]"
    pp (LR12 s) = "R12" ++ "[" ++ show s ++ "]"
    pp (LR13 s) = "R13" ++ "[" ++ show s ++ "]"
    pp (LR14 s) = "R14" ++ "[" ++ show s ++ "]"
    pp (LR15 s) = "R15" ++ "[" ++ show s ++ "]"
    pp (GI i s) = "g"++(show i) ++ "[" ++ show s ++ "]"
    pp (MEM s su)  = s ++ "[" ++ show su ++ "]"
    pp (SREG i s) = "s" ++ (show i) ++ "[" ++ show s ++ "]"
    treeify a = Node (pp a) []
