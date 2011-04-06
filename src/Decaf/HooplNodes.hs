module Decaf.HooplNodes where
import Compiler.Hoopl hiding (Top)
import Decaf.IR.LIR


-- Labels 
type HooplLabel = Int



-- nodes
data Node e x where
  LIRRegAssignNode :: LIRReg -> LIRExpr -> Node O O
  LIRIfNode        :: LIRRelExpr -> HooplLabel -> HooplLabel -> Node O C 
  LIRJumpLabelNode :: HooplLabel -> Node O C
  LIRLabelNode     :: HooplLabel -> Node C O
  -- add more
  LIRTempEnterNode :: Int -> Node O O   -- 
  LIRRegOffAssignNode :: LIRReg -> LIRReg -> LIRSize -> LIROperand -> Node O O
  LIRStoreNode     :: LIRMemAddr -> LIROperand -> Node O O
  LIRLoadNode      :: LIRReg -> LIRMemAddr -> Node O O
  
  -- these two not sure what they mean
  -- LIRRegCmpAssignInst LIRReg LIRExpr LIRLabel 
  -- LIRCondAssignInst LIRReg LIRReg LIROperand    -- ^ Conditional Assign

  -- these three should not be in hoopl graph
  -- LIRJumpRegInst LIRReg LIROffset
  -- LIRCallInst LIRProc
  -- LIRRetInst



