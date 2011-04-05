{-# LANGUAGE RankNTypes
  , MultiParamTypeClasses
  , FlexibleInstances
  , ImpredicativeTypes #-}

module Decaf.Assembler where
import Prelude hiding (not, div)
import Data.Int
import Data.Char
import Decaf.IR.SymbolTable
import Decaf.IR.LIR
import Decaf.IR.AST
import Decaf.IR.ASM

------------------- MONAD -----------------------------
data AssemblerState = AssemblerState
    { assembler       :: ASMList
    , declaredSymbols :: [String]
    , utilizedSymbols :: [String]
    , machine         :: MachineState
    }

data MachineState = MachineState
    { state :: [Int] }

newtype Assembler a = Assembler
    { runAssembler :: AssemblerState -> (a, AssemblerState) }

instance Monad Assembler where
    return a = Assembler (\s -> (a, s))
    m >>= f = Assembler (\s ->
        let (a, s') = runAssembler m s
        in runAssembler (f a) s')

mkAssemblerState :: AssemblerState
mkAssemblerState = AssemblerState ASMNil [] [] mkMachine

mkMachine :: MachineState
mkMachine = MachineState []

appendAssembler :: forall a. ASMInst a -> Assembler ()
appendAssembler item = Assembler (\s ->
    let updatedData = ASMCons item (assembler s)
    in ((), s { assembler = updatedData }))

appendSymbol :: String -> Assembler ()
appendSymbol item = Assembler (\s ->
    let updatedData = [item] ++ declaredSymbols s
    in ((), s { declaredSymbols = updatedData }))

declareSymbol :: String -> Assembler ()
declareSymbol id = appendSymbol id

getAsmList :: Assembler ASMList
getAsmList = Assembler (\s -> (assembler s, s))

mov :: ASMGenOperand -> ASMGenOperand -> Assembler ()
mov (ASMGenOperand op1) (ASMGenOperand op2) = appendAssembler (ASMMovInst op1 op2)

add :: ASMGenOperand -> ASMGenOperand -> Assembler ()
add (ASMGenOperand op1) (ASMGenOperand op2) = appendAssembler (ASMAddInst op1 op2)

sub :: ASMGenOperand -> ASMGenOperand -> Assembler ()
sub (ASMGenOperand op1) (ASMGenOperand op2) = appendAssembler (ASMSubInst op1 op2)

mul :: ASMGenOperand -> Assembler ()
mul (ASMGenOperand o) = appendAssembler (ASMMulInst o)

div :: ASMGenOperand -> Assembler ()
div (ASMGenOperand o) = appendAssembler (ASMDivInst o)

mod :: ASMGenOperand -> Assembler ()
mod (ASMGenOperand o) = appendAssembler (ASMModInst o)

push :: ASMGenOperand -> Assembler ()
push (ASMGenOperand o) = appendAssembler (ASMPushInst o)

pop :: ASMGenOperand -> Assembler ()
pop (ASMGenOperand o) = appendAssembler (ASMPopInst o)

jmp :: ASMLabel -> Assembler ()
jmp l = appendAssembler (ASMJmpInst l)

je :: ASMLabel -> Assembler ()
je l = appendAssembler (ASMJeInst l)

jne :: ASMLabel -> Assembler ()
jne l = appendAssembler (ASMJneInst l)

jg :: ASMLabel -> Assembler ()
jg l = appendAssembler (ASMJgInst l)

jge :: ASMLabel -> Assembler ()
jge l = appendAssembler (ASMJgeInst l)

jl :: ASMLabel -> Assembler ()
jl l = appendAssembler (ASMJlInst l)

jle :: ASMLabel -> Assembler ()
jle l = appendAssembler (ASMJleInst l)

neg :: ASMGenOperand -> Assembler ()
neg (ASMGenOperand o) = appendAssembler (ASMNegInst o)

not :: ASMGenOperand -> Assembler ()
not (ASMGenOperand o) = appendAssembler (ASMNotInst o)

cmp :: ASMGenOperand -> ASMGenOperand -> Assembler ()
cmp (ASMGenOperand op1) (ASMGenOperand op2) = appendAssembler (ASMCmpInst op1 op2)

enter :: Int -> Assembler ()
enter offset = appendAssembler (ASMEnterInst offset)

label :: ASMLabel -> Assembler ()
label l = appendAssembler (ASMLabelInst l)

call :: ASMSymbol -> Assembler ()
call symbol = appendAssembler (ASMCallInst symbol)

ret :: Assembler ()
ret = appendAssembler (ASMRetInst)

------------------- ASSEMBLER --------------------------
defaultFlags :: [ASMFlag]
defaultFlags =
    [ ASMFlag "USE64"
    ]

defaultExterns :: [ASMExternDecl]
defaultExterns =
    [ ASMExternDecl "printf"
    , ASMExternDecl "get_int_035"
    ]

programAssembler :: SymbolTable -> LIRProgram -> Assembler ASMProgram
programAssembler st prog =
    do dataSection <- mapDataSection st
       mapTextSection prog
       text <- getAsmList
       return (ASMProgram defaultFlags defaultExterns [dataSection, ASMTextSection text])

------------------- DATA --------------------------------

mapDataSection :: SymbolTable -> Assembler ASMSection
mapDataSection (SymbolTable records _) =
    do decs <- mapM mapDataDecl records
       let fdecs = filterDecls decs
       return (ASMDataSection fdecs)
  where
    filterDecls :: [Maybe ASMDataDecl] -> [ASMDataDecl]
    filterDecls [] = []
    filterDecls (x:xs) = case x of
                            Just y -> (y:filterDecls xs)
                            Nothing -> filterDecls xs

mapDataDecl :: SymbolRecord -> Assembler (Maybe ASMDataDecl)
mapDataDecl (VarRec (DecafVar ty ident _) num) =
    do declareSymbol id
       return $ Just (ASMCommonSegment (ASMLabel id) size)
  where
    id = "g" ++ show (-num - 1)
    size = 1

mapDataDecl (ArrayRec (DecafArr ty ident len _) go) =
    do declareSymbol id
       return $ Just (ASMCommonSegment (ASMLabel id) size)
  where
    id = arrayLabel go
    size = readDecafInteger len

mapDataDecl (StringRec str sl) = 
    do declareSymbol id
       return $ Just (ASMDataSegment (ASMLabel id) byteString)
  where
    id = stringLabel sl
    byteString = (map (ASMByte . ord) (str ++ ['\0']))

mapDataDecl other = return Nothing

------------------- TEXT --------------------------------

mapTextSection :: LIRProgram -> Assembler ASMSection
mapTextSection (LIRProgram (LIRLabel label) units) =
    do instructions <- mapM mapUnit units
       let head = ASMCons (ASMLabelInst (ASMLabel label)) ASMNil
           instructions' = castAsmList instructions
       return (ASMTextSection (asmConcat head instructions'))

mapUnit :: LIRUnit -> forall a. Assembler ASMList
mapUnit (LIRUnit (LIRLabel label) insts) =
    do instructions <- mapM labelFilter insts
       let head = unitLabel
           instructions' = castAsmList instructions
       return (asmConcat unitLabel instructions')
 where
    unitLabel = if (length label > 0)
                  then ASMCons (ASMLabelInst (ASMLabel label)) ASMNil
                  else ASMNil

    labelFilter x =
        case x of
            LIRLabelInst (LIRLabel "") -> return ASMNil
            other                      -> do mapInst other
                                             return ASMNil

------------------- INSTRUCTIONS --------------------------
mapInst :: LIRInst -> Assembler ()
mapInst (LIRRegAssignInst reg e@(LIRBinExpr (LIRRegOperand reg') binop op2)) =
        if reg == reg'
          then if reg == LRSP
                then case binop of
                    LSUB      -> do operOpen op2 1
                                    sub rsp r11
                    LADD      -> do operOpen op2 1
                                    add rsp r11 
                    otherwise -> genExpr reg e
                else genExpr reg e
          else genExpr reg e

mapInst (LIRRegAssignInst reg expr@(LIRBinExpr op1 binop op2)) =
        genExpr reg expr

mapInst (LIRRegAssignInst reg (LIRUnExpr LNEG operand)) =
    do mov' reg operand
       neg (asm (LIRRegOperand reg))

mapInst (LIRRegAssignInst reg (LIRUnExpr LNOT operand)) =
    do mov' reg operand
       not (asm (LIRRegOperand reg))

mapInst (LIRRegAssignInst reg (LIROperExpr operand)) =
    case reg of
        LRSP      -> movaddr mem operand
        otherwise -> mov' reg operand
  where
    mem = LIRMemAddr LRSP Nothing (LIRInt 0) (LIRInt 8)

mapInst (LIRRegOffAssignInst reg reg' size operand) = 
    do operOpen operand 1
       mov mem r11
  where
    mem = asm (LIRMemAddr reg (Just reg') (LIRInt 0) size)

mapInst (LIRStoreInst mem operand) =
        movaddr mem operand 

--mapInst (LIRLoadInst reg mem@(LIRRegPlusMemAddr arr offset (LIRInt size))) = 
--        do operOpen offset 1
--           mov r10 (asm mem)
--           regSave reg
        
mapInst (LIRLoadInst reg mem) =
        mov' reg mem

mapInst (LIRJumpLabelInst (LIRLabel l)) =
        jmp (ASMLabel l)

mapInst (LIRIfInst (LIRBinRelExpr op1 binop op2) (LIRLabel l)) =
    do cmp' op1 op2
       jtype binop l

mapInst (LIRIfInst (LIRNotRelExpr operand) (LIRLabel l)) =
    do cmp' operand (LIRIntOperand asmFalse)
       jl (ASMLabel l)

mapInst (LIRIfInst (LIROperRelExpr operand) (LIRLabel l)) =
    do cmp' operand (LIRIntOperand $ asmFalse)
       jl (ASMLabel l)

mapInst (LIRCallInst (LIRProcLabel  proc)) =
    call (ASMSymbol proc)

mapInst (LIRTempEnterInst s) =
    enter (s * 8)

mapInst LIRRetInst =
    do mov rsp rbp
       pop rbp
       ret

mapInst (LIRLabelInst (LIRLabel l)) = label (ASMLabel l)

regOpen :: LIRReg -> Assembler ()
regOpen r = 
    mov r10 (asm r)

regSave :: LIRReg -> Assembler ()
regSave r = mov (asm r) r10

operOpen :: ASMOper a => a -> Int -> Assembler ()
operOpen op num =
    mov reg (asm op)
  where
    reg = case num of
              0 -> r10
              1 -> r11

movaddr :: ASMOper a => LIRMemAddr -> a -> Assembler ()
movaddr addr op2 =
  do operOpen op2 1
     mov (asm addr) r11

--twoop :: Assembler m -> LIROperand -> LIROperand -> Assembler ()
twoop opcode o1@(SREG {}) o2 =
    do operOpen o2 1
       opcode r10 r11
       regSave o1

twoop opcode o1 o2 =
    do operOpen o2 1
       opcode (asm o1) r11

mov' op1 op2 = twoop mov op1 op2
add' op1 op2 = twoop add op1 op2
sub' op1 op2 = twoop sub op1 op2
cmp' op1 op2 =
    do operOpen op1 0
       operOpen op2 1
       cmp r10 r11

mul' op =
    do operOpen op 0
       mul r10

div' op =
    do operOpen op 0
       div r10

mod' op =
    do operOpen op 0
       div r10

jtype :: LIRRelOp -> String -> Assembler ()
jtype binop l' = opcode >> return ()
  where
    l = ASMLabel l'
    opcode = case binop of
                  LEQ -> je l
                  LNEQ -> jne l
                  LGT -> jg l
                  LGTE -> jge l
                  LLT -> jl l
                  LLTE -> jl l

genExpr :: LIRReg -> LIRExpr -> Assembler ()
genExpr reg expr =
  case expr of
      LIRBinExpr op1' LSUB op2' -> do mov' LR10 op1' 
                                      sub' LR10 op2'
                                      regSave reg

      LIRBinExpr op1' LADD op2' -> do mov' LR10 op1'
                                      add' LR10 op2'
                                      regSave reg

      LIRBinExpr op1' LMUL op2' -> do mov' LRAX op1'
                                      mul' op2'
                                      mov (asm reg) rax

      LIRBinExpr op1' LDIV  op2' -> do mov' LR9 LRDX
                                       mov' LRDX (LIRIntOperand $ LIRInt 0) -- must be 0
                                       mov' LRAX op1'
                                       div' op2'
                                       mov (asm reg) rax
                                       mov' LRDX r9

      LIRBinExpr op1' LMOD  op2' -> do mov' LR9 LRDX
                                       mov' LRDX (LIRIntOperand $ LIRInt 0) --must be 0
                                       mov' LRAX op1'
                                       div' op2'
                                       mov (asm reg) rdx
                                       mov' LRDX LR9

      LIRBinExpr op1' (LIRBinRelOp binop (LIRLabel l)) op2' ->
            do cmp' op1' op2'
               jtype binop l
               mov' reg (LIRIntOperand asmFalse)
               jmp endl
               mov' reg (LIRIntOperand asmTrue)
               label endl
        where
          endl = ASMLabel (l ++ "_end")

mapRegister :: LIRReg -> ASMReg
mapRegister reg =
    case reg of
            LRAX -> RAX
            LRBX -> RBX
            LRCX -> RCX
            LRDX -> RDX
            LRBP -> RBP
            LRSP -> RSP
            LRSI -> RSI
            LRDI -> RDI
            LR8 -> R8
            LR9 -> R9
            LR10 -> R10
            LR11 -> R11
            LR12 -> R12
            LR13 -> R13
            LR14 -> R14
            LR15 -> R15
            MEM s -> R15
            GI s -> R15
            SREG s -> R15

class ASMOper a where
    asm :: a -> ASMGenOperand

instance ASMOper LIROperand where
    asm (LIRRegOperand reg)  = ASMGenOperand $ ASMRegOperand (mapRegister reg) 8 -- TODO figure out if it's safe to assume qword
    asm (LIRIntOperand (LIRInt i)) = ASMGenOperand $ ASMLitOperand i
    asm (LIRStringOperand s) = ASMGenOperand $ ASMSymbolOperand (ASMSymbol s) 0 0

instance ASMOper LIRMemAddr where
    asm (LIRMemAddr base reg (LIRInt offset) (LIRInt size)) =
        ASMGenOperand $ ASMMemAddr (mapRegister base) (fmap mapRegister reg) offset size

instance ASMOper ASMGenOperand where
    asm = id

instance ASMOper LIRReg where
    asm r = ASMGenOperand $ ASMRegOperand (mapRegister r) 8
