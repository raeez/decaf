{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}

module Decaf.LIRNodes where
import Decaf.IR.LIR
import Decaf.IR.IRNode
import Data.Int
import Loligoptl
import qualified Loligoptl.Label

-- Graph
type LIRGraph = Graph LIRNode

-- nodes
data LIRNode e x where
  LIRLabelNode        :: Label -> LIRNode C O
  LIRRegAssignNode    :: LIRReg -> LIRExpr -> LIRNode O O
  LIRRegOffAssignNode :: LIRReg -> LIRReg -> LIRSize -> LIROperand -> LIRNode O O
  LIRStoreNode        :: LIRMemAddr -> LIROperand -> LIRNode O O
  LIRLoadNode         :: LIRReg -> LIRMemAddr -> LIRNode O O
  LIRCallNode         :: LIRLabel -> LIRLabel -> LIRNode O C -- method label and label for next line
  LIRCalloutNode      :: String -> LIRNode O O -- assume control falls through
  LIREnterNode        :: LIRInt -> LIRNode O O
  LIRRetNode          :: [LIRLabel] -> String -> LIRNode O C
  LIRIfNode           :: LIRRelExpr -> Label -> Label -> LIRNode O C  -- false, then true
  LIRJumpLabelNode    :: Label -> LIRNode O C
    
instance NonLocal LIRNode where
  entryLabel (LIRLabelNode lab) = lab
  successors (LIRRetNode labels _) = labels
  successors (LIRCallNode l1 l2) = [l1, l2]
  successors (LIRJumpLabelNode lab) = [lab]
  successors (LIRIfNode expr flab tlab) = [flab, tlab]

-- node to G
nodeToG :: forall e x. (ShapeLifter e x) => LIRNode e x -> Graph LIRNode e x
nodeToG n = singletonG n

-- join two change flag
joinChangeFlag :: ChangeFlag -> ChangeFlag -> ChangeFlag 
joinChangeFlag NoChange NoChange = NoChange
joinChangeFlag _ _               = SomeChange                             


instance Show (LIRNode e x) where
  show (LIRLabelNode label)                       = pp $ LIRLabelInst label
  show (LIRRegAssignNode reg expr)                = pp $ LIRRegAssignInst reg expr
  show (LIRRegOffAssignNode reg reg' size operand) = "LIRRegOffAssignNode"
  show (LIRStoreNode mem operand)                  = "LIRStoreNode"
  show (LIRLoadNode reg mem)                       = "LIRLoadNode" 
  show (LIRCallNode label label')                  = "LIRCallNode"
  show (LIRCalloutNode label)                     = "LIRCalloutNode"
  show (LIREnterNode int)                          = "LIREnterNode" 
  show (LIRRetNode labels meth)                    = "LIRRetNode" 
  show (LIRIfNode expr label1 label2)              = "LIRIfNode"
  show (LIRJumpLabelNode label)                    = "LIRJumpLabelNode"
