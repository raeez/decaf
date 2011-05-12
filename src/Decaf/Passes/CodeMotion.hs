module Decaf.Passes.CodeMotion (loopInvariant) where
import Decaf.IR.LIR

loopInvariant :: LIRReg -> CFGInst -> Bool
loopInvariant r (CFGLIRInst (LIRRegAssignInst reg e))
    = (loopInvariantReg r reg) && (loopInvariantExpr r e)
loopInvariant r (CFGLIRInst (LIRRegOffAssignInst reg1 reg2 size oper))
    = (loopInvariantReg r reg1) && (loopInvariantReg r reg2) && (loopInvariantOper r oper)
loopInvariant r (CFGLIRInst (LIRStoreInst mem oper))
    = (loopInvariantMem r mem) && (loopInvariantOper r oper)
loopInvariant r (CFGLIRInst (LIRLoadInst reg mem))
    = (loopInvariantReg r reg) && (loopInvariantMem r mem)
loopInvariant r (CFGLIRInst _) = False
loopInvariant r _ = False

loopInvariantMem :: LIRReg -> LIRMemAddr -> Bool
loopInvariantMem r (LIRMemAddr reg1 (Just reg2) _ _)
    = (loopInvariantReg r reg1) && (loopInvariantReg r reg2)
loopInvariantMem r (LIRMemAddr reg1 Nothing _ _)
    = loopInvariantReg r reg1

loopInvariantExpr :: LIRReg -> LIRExpr -> Bool
loopInvariantExpr r (LIRBinExpr o1 _ o2)
    = (loopInvariantOper r o1) && (loopInvariantOper r o2)
loopInvariantExpr r (LIRUnExpr _ o)
    = loopInvariantOper r o
loopInvariantExpr r (LIROperExpr o)
    = loopInvariantOper r o

loopInvariantOper :: LIRReg -> LIROperand -> Bool
loopInvariantOper r (LIRRegOperand reg) = False -- loopInvariantReg r reg
loopInvariantOper _ _ = True

loopInvariantReg :: LIRReg -> LIRReg -> Bool
loopInvariantReg r r' =  False
