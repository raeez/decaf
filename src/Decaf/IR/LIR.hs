{-# LANGUAGE DeriveDataTypeable
  , TypeSynonymInstances #-}

module Decaf.IR.LIR where
import Numeric
import Data.Int
import Decaf.IR.Class
import Decaf.Data.Tree
import Data.Typeable
import Decaf.IR.SymbolTable

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
exceptionHeader = LIRLabel "__exceptionhandlers"

exceptionString :: SymbolTree -> String -> String
exceptionString st msg =
  case globalSymLookup ('.':msg) st of
      Just (StringRec _ l) ->  "__string" ++ show l
      _                    ->
          error $ "LIR.hs:exceptionString could not find symbol for :" ++msg 

exceptionLabel :: SymbolTree -> String -> LIRLabel
exceptionLabel st msg  = case globalSymLookup ('.':msg) st of
                           Just (StringRec _ l) ->  LIRLabel $ "__exception" ++ show l
                           _ -> error $ "LIR.hs:exceptionLabel could not find symbol for :" ++ msg

boundsLabel :: Bool -> Int -> LIRLabel
boundsLabel t c = if t then (LIRLabel $ "__boundscheck" ++ show c) else (LIRLabel $ "__boundscheck" ++ show c ++ "__positive")

compareLabel :: Int -> LIRLabel
compareLabel c = LIRLabel $  "__cmp" ++ show c

stringLabel :: Int -> String
stringLabel c = "__string" ++ show c

arrayLabel :: Int -> String
arrayLabel c = "__array" ++ show c

methodLabel :: String -> Int -> String
methodLabel m c = "__proc" ++ show c ++ "__" ++ m

loopLabel :: Int -> LIRLabel
loopLabel l = LIRLabel $ "LLOOP" ++ show l

endLabel :: Int -> LIRLabel
endLabel l = LIRLabel $ "LEND" ++ show l

trueLabel :: Int -> LIRLabel
trueLabel l = LIRLabel $ "LTRUE" ++ show l

data LIRProgram = LIRProgram
    { lirProgLabel :: LIRLabel
    , lirProgUnits :: [LIRUnit]
    } deriving (Show, Eq, Typeable)

data LIRUnit = LIRUnit
    { lirUnitLabel ::LIRLabel
    , lirUnitInstructions :: [LIRInst]
    } deriving (Show, Eq, Typeable)

data LIRInst = LIRRegAssignInst LIRReg LIRExpr
             | LIRRegCmpAssignInst LIRReg LIRExpr LIRLabel
             | LIRRegOffAssignInst LIRReg LIRReg LIRSize LIROperand
             | LIRStoreInst LIRMemAddr LIROperand
             | LIRLoadInst LIRReg LIRMemAddr
             | LIREnterInst Int64
             | LIRJumpLabelInst LIRLabel
             | LIRIfInst LIRRelExpr LIRLabel
             | LIRCallInst LIRProc
             | LIRRetInst
             | LIRLabelInst LIRLabel
             deriving (Show, Eq, Typeable)


data LIRProc = LIRProcLabel String
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
              deriving (Show, Eq, Typeable)

data LIRUnOp = LNEG
             | LNOT
             deriving (Show, Eq, Typeable)

data LIRRelOp = LEQ
              | LNEQ
              | LGT
              | LGTE
              | LLT
              | LLTE
              deriving (Show, Eq, Typeable)

data LIRMemAddr = LIRMemAddr LIRReg (Maybe LIRReg) LIROffset LIRSize
                deriving (Show, Eq, Typeable)

data LIROperand = LIRRegOperand LIRReg
                | LIRIntOperand LIRInt
                | LIRStrOperand String
                deriving (Show, Eq, Typeable)

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
            deriving (Show, Eq, Typeable)

type LIRSize = LIRInt

type LIROffset = LIRInt

type LIRInt = Int64

data LIRLabel = LIRLabel String
              deriving (Show, Eq, Typeable)

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
    pp (LIRRegCmpAssignInst reg expr label) = pp reg ++ " <- " ++ pp expr ++ " [" ++ pp label ++ "]"
    pp (LIRRegOffAssignInst reg offset size operand) = pp reg ++ "(" ++ pp offset ++ ", " ++ pp size ++ ") <- " ++ pp operand
    pp (LIRStoreInst mem operand) = "STORE " ++ pp mem ++ ", " ++ pp operand
    pp (LIRLoadInst reg mem) = "LOAD " ++ pp reg ++ ", " ++ pp mem
    pp (LIREnterInst num) = "ENTER " ++ (show num)
    pp (LIRJumpLabelInst label) = "JMP " ++ pp label
    pp (LIRIfInst expr label) = "IF " ++ pp expr ++ " JMP " ++ pp label
    pp (LIRCallInst proc) = "call " ++ pp proc
    pp LIRRetInst = "RET"
    pp (LIRLabelInst label) = pp label
    treeify (LIRRegAssignInst reg expr) = Node "ASSIGN" [treeify reg, treeify expr]
    treeify (LIRRegOffAssignInst reg offset size operand) = Node "ASSIGN" [treeify reg, treeify offset, treeify size, treeify operand]
    treeify (LIRStoreInst mem operand) = Node "STR" [treeify mem, treeify operand]
    treeify (LIRLoadInst reg mem) = Node "LD" [treeify reg, treeify mem]
    treeify (LIRJumpLabelInst label) = Node "JMP" [treeify label]
    treeify (LIRIfInst expr label) = Node "IF" [treeify expr, treeify label]
    treeify (LIRCallInst proc) = Node "CALL" [treeify proc]
    treeify LIRRetInst = Node "RET" []
    treeify (LIRLabelInst label) = Node (pp label) []
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
    pp (LIRBinRelOp relop label) = pp relop
    treeify (LIRBinRelOp relop label) = treeify relop
    treeify a = Node (pp a) []
    pos _     = error "LIR has no associated position"

instance IRNode LIRUnOp where
    pp (LNEG) = "-"
    pp (LNOT) = "!"
    treeify a = Node (pp a) []
    pos _     = error "LIR has no associated position"

instance IRNode LIRRelOp where
    pp (LEQ) = "=="
    pp (LNEQ) = "!="
    pp (LGT) = ">"
    pp (LGTE) = ">="
    pp (LLT) = "<"
    pp (LLTE) = "<="
    treeify a = Node (pp a) []
    pos _     = error "LIR has no associated position"

instance IRNode LIRMemAddr where
    pp (LIRMemAddr base reg offset size) =
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

    pos _     = error "LIR has no associated position"

instance IRNode LIROperand where
    pp (LIRRegOperand reg) = pp reg
    pp (LIRIntOperand i) = pp i
    pp (LIRStrOperand s) = s
    treeify a = Node (pp a) []
    pos _     = error "LIR has no associated position"

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
    pos _     = error "LIR has no associated position"

instance IRNode LIRInt where
    pp i = (if i < 0 then "-" else "") ++ "0x" ++ showHex (abs i) ""
    treeify i = Node (pp i) []
    pos _     = error "LIR has no associated position"

instance IRNode LIRLabel where
    pp (LIRLabel s) = s
    treeify s = Node (pp s) []
    pos _     = error "LIR has no associated position"
