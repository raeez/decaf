{-# LANGUAGE RankNTypes
  , MultiParamTypeClasses
  , FlexibleInstances
  , ImpredicativeTypes
  , GADTs #-}

module Decaf.Assembler where
import Prelude hiding (not, div, mapM)
import Data.Int
import Data.Char
import Data.Set hiding (map)
import Data.Traversable
import Decaf.IR.SymbolTable
import Decaf.IR.LIR
import Decaf.IR.AST
import Decaf.IR.ASM

------------------- MONAD -----------------------------
data AssemblerState = AssemblerState
    { assembler       :: ASMInstructions
    , declaredSymbols :: Set ASMExternDecl
    , utilizedSymbols :: Set ASMExternDecl
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
mkAssemblerState = AssemblerState ASMNil empty empty mkMachine

mkMachine :: MachineState
mkMachine = MachineState []

appendAssembler :: forall a. ASMInst a -> Assembler ()
appendAssembler item = Assembler (\s ->
    let updatedData = ASMCons item (assembler s)
    in ((), s { assembler = updatedData }))


declareSymbol :: String -> Assembler ()
declareSymbol sym = Assembler (\s ->
    let updatedData = insert (ASMExternDecl sym) (declaredSymbols s)
    in ((), s { declaredSymbols = updatedData }))

useSymbol :: String -> Assembler ()
useSymbol sym = Assembler (\s ->
    let updatedData = insert (ASMExternDecl sym) (utilizedSymbols s)
    in ((), s { utilizedSymbols = updatedData }))

getAsmList :: Assembler ASMInstructions
getAsmList = Assembler (\s -> (assembler s, s))

getExterns :: Assembler (Set ASMExternDecl)
getExterns = Assembler (\s -> (externs s, s))
  where
    externs s = difference (utilizedSymbols s) (declaredSymbols s) 

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

enter :: Int64 -> Assembler ()
enter offset = appendAssembler (ASMEnterInst offset)

label :: ASMLabel -> Assembler ()
label l = appendAssembler (ASMLabelInst l)

call :: ASMSym -> Assembler ()
call symbol = appendAssembler (ASMCallInst symbol)

ret :: Assembler ()
ret = appendAssembler (ASMRetInst)

------------------- ASSEMBLER --------------------------
defaultFlags :: Set ASMFlag
defaultFlags =
    fromList flags
  where
    flags =
        [ ASMFlag "USE64"
        ]

defaultExterns :: Set ASMExternDecl
defaultExterns =
    fromList externs
  where
    externs =
        [ ASMExternDecl "printf"
        , ASMExternDecl "get_int_035"
        ]

programAssembler :: SymbolTable -> LIRProgram -> Assembler ASMProgram
programAssembler st prog =
    do -- generate data section
       dataSection <- mapDataSection st
       -- generate text section
       mapTextSection prog
       text <- getAsmList
       -- generate externs
       externs <- getExterns
       return (ASMProgram defaultFlags (union defaultExterns externs) [dataSection, ASMTextSection text])

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
    do declareSymbol sym
       return $ Just (ASMCommonSegment (ASMLabel "g" (-num -1)) size)
  where
    sym = "g" ++ show (-num - 1)
    size = 1

mapDataDecl (ArrayRec (DecafArr ty ident len _) go) =
    do declareSymbol sym
       return $ Just (ASMCommonSegment (ASMLabel "__array" go) size)
  where
    sym = arrayLabel go
    size = readDecafInteger len

mapDataDecl (StringRec str sl) = 
    do declareSymbol sym
       return $ Just (ASMDataSegment (ASMLabel "__string" sl) byteString)
  where
    sym = stringLabel sl
    byteString = (map (ASMByte . fromIntegral . ord) (str ++ ['\0']))

mapDataDecl (MethodRec m (_, i)) =
    do declareSymbol $ show $ methodLabel (methodID m) i
       return Nothing

------------------- TEXT --------------------------------

mapTextSection :: LIRProgram -> Assembler ASMSection
mapTextSection (LIRProgram (LIRLabel label i) units) =
    do instructions <- mapM mapUnit units
       let head = ASMCons (ASMLabelInst (ASMLabel label i)) ASMNil
           instructions' = castAsmList instructions
       return $ ASMTextSection (asmConcat head instructions')

mapUnit :: LIRUnit -> forall a. Assembler ASMInstructions
mapUnit (LIRUnit (LIRLabel label i) insts) =
    do instructions <- mapM labelFilter insts
       let head = unitLabel
           instructions' = castAsmList instructions
       return (asmConcat unitLabel instructions')
 where
    unitLabel = if (length label > 0)
                  then ASMCons (ASMLabelInst (ASMLabel label i)) ASMNil
                  else ASMNil

    labelFilter x =
        case x of
            LIRLabelInst (LIRLabel "" _) -> return ASMNil
            other                      -> do mapInst other
                                             return ASMNil

------------------- INSTRUCTIONS --------------------------
mapInst :: LIRInst -> Assembler ()
mapInst (LIRRegAssignInst reg e@(LIRBinExpr (LIRRegOperand reg') binop op2)) =
        if reg == reg'
          then if reg == LRSP
                then case binop of
                    LSUB      -> do o2 <- asm op2
                                    mov r11 o2
                                    sub rsp r11
                    LADD      -> do o2 <- asm op2
                                    mov r11 o2
                                    add rsp r11 
                    otherwise -> genExpr reg e
                else genExpr reg e
          else genExpr reg e

mapInst (LIRRegAssignInst reg expr@(LIRBinExpr op1 binop op2)) =
        genExpr reg expr

mapInst (LIRRegAssignInst reg (LIRUnExpr LNEG operand)) =
    do r <- asm reg
       o <- asm operand
       mov' r o

       r <- asm $ LIRRegOperand reg
       neg r

mapInst (LIRRegAssignInst reg (LIRUnExpr LNOT operand)) =
    do r <- asm reg
       o <- asm operand
       mov' r o

       r <- asm $ LIRRegOperand reg
       not r

mapInst (LIRRegAssignInst reg (LIROperExpr operand)) =
    case reg of
        LRSP      -> do m <- asm mem
                        o <- asm operand
                        loadstore m o
        otherwise -> do r <- asm reg
                        o <- asm operand
                        mov' r o
  where
    mem = LIRMemAddr LRSP Nothing 0 8

mapInst (LIRRegOffAssignInst reg reg' size operand) = 
    do mov' mem operand
  where
    mem = LIRMemAddr reg (Just reg') 0 size

mapInst (LIRStoreInst mem operand) =
    do m <- asm mem
       o <- asm operand
       loadstore m o

mapInst (LIRLoadInst reg mem) =
    do r <- asm reg
       m <- asm mem
       loadstore r m

mapInst (LIRJumpLabelInst (LIRLabel l i)) =
        jmp (ASMLabel l i)

-- following two functions only depend on true label (false block requires no immediate jump)
mapInst (LIRIfInst (LIRBinRelExpr op1 binop op2) (LIRLabel fll fli) tlab@(LIRLabel tll tli)) =
    do o1 <- asm op1
       o2 <- asm op2
       cmp' o1 o2
       jumpType binop tlab -- not sure if this is right raeez
       jmp (ASMLabel fll fli)

mapInst (LIRIfInst (LIRNotRelExpr operand) (LIRLabel fll fli) (LIRLabel tll tli)) =
    do o <- asm operand
       cmp' o (lit litFalse)
       jl (ASMLabel tll tli)
       jmp (ASMLabel fll fli)

mapInst (LIRIfInst (LIROperRelExpr operand) (LIRLabel fll fli) (LIRLabel tll tli)) =
    do o <- asm operand
       cmp' o (lit litFalse)
       jl (ASMLabel tll tli)
       jmp (ASMLabel fll fli)

mapInst (LIRCallInst lab (LIRLabel l i)) =
    do useSymbol (show lab)
       call (ASMSym (show lab))
       jmp (ASMLabel l i)

mapInst (LIRCalloutInst lab) =
    do useSymbol lab
       call (ASMSym lab)

mapInst (LIREnterInst s) =
    do push rbp
       mov rbp rsp
       sub rsp (lit $ s * 8)

mapInst LIRRetInst =
    do leave
       ret
mapInst (LIRLabelInst (LIRLabel l i)) = label (ASMLabel l i)

leave :: Assembler ()
leave = do mov rsp rbp
           pop rbp

loadstore :: ASMGenOperand -> ASMGenOperand -> Assembler ()
loadstore reg@(ASMGenOperand ASMRegOperand {}) addr@(ASMGenOperand ASMMemOperand {}) =
    mov reg addr

loadstore addr@(ASMGenOperand ASMMemOperand {}) reg@(ASMGenOperand ASMRegOperand {}) =
    mov addr reg

loadstore addr1@(ASMGenOperand ASMMemOperand {}) addr2@(ASMGenOperand ASMMemOperand {}) =
    do mov r10 addr2
       mov addr1 r10

loadstore addr@(ASMGenOperand ASMMemOperand {}) pntr@(ASMGenOperand ASMSymOperand {}) =
    do mov r10 pntr
       mov addr r10

loadstore reg@(ASMGenOperand ASMRegOperand {}) pntr@(ASMGenOperand ASMSymOperand {}) =
    mov reg pntr

loadstore op1 op2 =
    mov op1 op2

twoop opcode op1 op2 =
    do o2 <- asm op2
       mov r11 o2

       o1 <- asm op1
       opcode o1 r11

mov' op1 op2 = twoop mov op1 op2
add' op1 op2 = twoop add op1 op2
sub' op1 op2 = twoop sub op1 op2
cmp' op1 op2 =
    do mov r10 op1
       mov r11 op2
       cmp r10 r11

mul' op =
    do mov r10 op
       mul r10

div' op =
    do mov r10 op
       div r10

mod' op =
    do mov r10 op
       div r10

jumpType :: LIRRelOp -> LIRLabel -> Assembler ()
jumpType binop (LIRLabel l' i) = opcode >> return ()
  where
    l = ASMLabel l' i
    opcode = case binop of
                  LEQ -> je l
                  LNEQ -> jne l
                  LGT -> jg l
                  LGTE -> jge l
                  LLT -> jl l
                  LLTE -> jle l

genExpr :: LIRReg -> LIRExpr -> Assembler ()
genExpr reg expr =
  case expr of
      LIRBinExpr op1' LSUB op2' -> do o1 <- asm op1'
                                      mov' r10  o1

                                      o2 <- asm op2'
                                      sub' r10 o2

                                      r <- asm reg
                                      mov r r10

      LIRBinExpr op1' LADD op2' -> do o1 <- asm op1'
                                      mov' r10 o1

                                      o2 <- asm op2'
                                      add' r10 o2
                                      r <- asm reg
                                      mov r r10

      LIRBinExpr op1' LMUL op2' -> do o1 <- asm op1'
                                      mov' rax o1

                                      o2 <- asm op2'
                                      mul' o2

                                      r <- asm reg
                                      mov r rax

      LIRBinExpr op1' LDIV  op2' -> do mov' r9 rdx
                                       mov' rdx (lit 0) -- must be 0

                                       o1 <- asm op1'
                                       mov' rax o1

                                       o2 <- asm op2'
                                       div' o2

                                       r <- asm reg
                                       mov r rax
                                       mov' rdx r9

      LIRBinExpr op1' LMOD  op2' -> do mov' r9 rdx
                                       mov' rdx (lit 0) --must be 0

                                       o1 <- asm op1'
                                       mov' rax o1

                                       o2 <- asm op2'
                                       div' o2

                                       o1 <- asm reg
                                       mov o1 rdx

                                       mov' rdx r9

      LIRBinExpr op1' (LIRBinRelOp binop lab@(LIRLabel l i)) op2' ->
        do op1 <- asm op1'
           op2 <- asm op2'
           cmp' op1 op2

           jumpType binop lab

           op1 <- asm reg
           mov' op1 (lit litFalse)

           jmp endl

           label (ASMLabel l i)

           op1 <- asm reg
           mov' op1 (lit litTrue)
           jmp endl

           label endl
        where
          endl = ASMLabel (l ++ "_end") i


class ASMOper a where
    asm :: a -> Assembler ASMGenOperand

instance ASMOper LIROperand where
    asm (LIRRegOperand reg) =
        return $ genOperand reg
      where
        genOperand reg =
            case reg of
                MEM s  -> ASMGenOperand $ ASMSymOperand (ASMSym s)
                GI s   ->  ASMGenOperand $ ASMMemOperand (ASMSymBase $ ASMSym $ "g" ++ show s) Nothing 0 8
                SREG s -> ASMGenOperand $ ASMMemOperand (ASMRegBase RBP) Nothing address 8
                  where
                    address =  fromIntegral (-8 * (s+1)) :: ASMInt
                other  -> ASMGenOperand $ ASMRegOperand (mapRegister reg) 8
      
    asm (LIRIntOperand i) = return $ ASMGenOperand $ ASMLitOperand i
    asm (LIRStrOperand s) = return $ ASMGenOperand $ ASMSymOperand (ASMSym s)

instance ASMOper LIRMemAddr where
    asm (LIRMemAddr (SREG s) reg offset size) =
        do r <- Data.Traversable.sequence $ fmap mapMemBase reg
           return $ ASMGenOperand $ ASMMemOperand (ASMRegBase RBP) r (offset + (stackAddress s)) size
    asm (LIRMemAddr base reg offset size) =
        do b <- mapMemBase base
           r <- Data.Traversable.sequence $ fmap mapMemBase reg
           return $ ASMGenOperand $ ASMMemOperand b r offset size

instance ASMOper ASMGenOperand where
    asm op = return op

instance ASMOper LIRReg where
    asm reg = case reg of
        SREG s -> return $ ASMGenOperand $ ASMMemOperand (ASMRegBase RBP) Nothing (stackAddress s) 8
        MEM s  -> return $ ASMGenOperand $ ASMSymOperand (ASMSym s)
        GI s   -> do r <- mapMemBase reg
                     return $ ASMGenOperand $ ASMMemOperand r Nothing 0 8
        other  -> return $ ASMGenOperand $ ASMRegOperand (mapRegister reg) 8

mapMemBase reg =
    case reg of
        MEM s   -> return $ ASMSymBase (ASMSym s)
        GI s    -> return $ ASMSymBase (ASMSym $ "g" ++ show s)
        SREG s  -> do let stackVar = ASMGenOperand $ ASMMemOperand (ASMRegBase RBP) Nothing (stackAddress s) 8
                      mov r10 stackVar
                      return $ ASMRegBase R10
        other   -> return $ ASMRegBase (mapRegister reg)

stackAddress :: Int -> ASMInt
stackAddress s = fromIntegral $ -8 * (s + 1) :: ASMInt

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
