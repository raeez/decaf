module Decaf.Translate where
import Decaf.Data.SymbolTable
import Decaf.IR.Class
import Decaf.IR.AST
import Decaf.IR.LIR

-- | Given a SymbolTree, Translate a complete AST into LIR
-- TODO need to handle recursive decent of st
translateProgram ::  SymbolTree -> DecafProgram -> LIRProgram
translateProgram st program = LIRProgram (LIRLabel "TODO") units
  where
    units = map (translateMethod st) (methods program)

-- | Given a SymbolTree, Translate a single DecafMethod into LIR program units
-- TODO need to handle recursive decent of st
translateMethod :: SymbolTree -> DecafMethod -> LIRUnit
translateMethod symboltree method = LIRUnit (LIRLabel "TODO") insts
  where
    insts = concatMap (translateStm st) (blockStms $ methodBody method)

-- | Given a SymbolTree, Translate a single DecafStatement
-- into a series of LIR instructions
translateStm :: SymbolTree -> DecafStm -> [LIRInst]
translateStm st (DecafAssignStm loc op expr _) = 
translateStm st (DecafMethodStm (DecafPureMethodCall mid args _) _) = 
translateStm st (DecafMethodStm (DecafMethodCallout mid args _) _) = 
translateStm st (DecafIfStm expr block elseblock _) = 
translateStm st (DecafRetStm (Just expr) _) = [LIRRetOperInst (translateExpr expr)]
translateStm st (DecafRetStm Nothing _) = [LIRRetInst]
translateStm st (DecafBreakStm _) = 
translateStm st (DecafContStm _) = 
translateStm st (DecafBlockStm block _) = 

translateExpr :: SymbolTree -> DecafExpr -> LIROperand
translateExpr st expr = 
