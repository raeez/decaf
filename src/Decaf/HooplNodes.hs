{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}

module Decaf.HooplNodes where
import Decaf.IR.LIR
import Decaf.IR.IRNode
import Data.Int
import Loligoptl
import qualified Loligoptl.Label

-- Graph
type DecafGraph = Graph Node

-- Labels 
type HooplLabel = Loligoptl.Label.Label

-- nodes
data Node e x where
  LIRLabelNode        :: HooplLabel -> Node C O
  LIRRegAssignNode    :: LIRReg -> LIRExpr -> Node O O
  LIRRegOffAssignNode :: LIRReg -> LIRReg -> LIRSize -> LIROperand -> Node O O
  LIRStoreNode        :: LIRMemAddr -> LIROperand -> Node O O
  LIRLoadNode         :: LIRReg -> LIRMemAddr -> Node O O
  LIRCallNode         :: LIRLabel -> LIRLabel -> Node O C -- method label and label for next line
  LIRCalloutNode      :: String -> Node O O -- assume control falls through
  LIREnterNode        :: LIRInt -> Node O O
  LIRRetNode          :: [LIRLabel] -> String -> Node O C
  LIRIfNode           :: LIRRelExpr -> HooplLabel -> HooplLabel -> Node O C  -- false, then true
  LIRJumpLabelNode    :: HooplLabel -> Node O C
    
instance NonLocal Node where
  entryLabel (LIRLabelNode lab) = lab
  successors (LIRRetNode labels _) = labels
  successors (LIRCallNode l1 l2) = [l1, l2]
  successors (LIRJumpLabelNode lab) = [lab]
  successors (LIRIfNode expr flab tlab) = [flab, tlab]

-- node to G
nodeToG :: forall e x. (ShapeLifter e x) => Node e x -> Graph Node e x
nodeToG n = singletonG n

-- join two change flag
joinChangeFlag :: ChangeFlag -> ChangeFlag -> ChangeFlag 
joinChangeFlag NoChange NoChange = NoChange
joinChangeFlag _ _               = SomeChange                             


instance Show (Node e x) where
  show (LIRLabelNode label )                       = pp $ LIRLabelInst label
  show (LIRRegAssignNode reg expr )                = pp $ LIRRegAssignInst reg expr
  show (LIRRegOffAssignNode reg reg' size operand) = "LIRRegOffAssignNode"
  show (LIRStoreNode mem operand)                  = "LIRStoreNode"
  show (LIRLoadNode reg mem)                       = "LIRLoadNode" 
  show (LIRCallNode label label')                  = "LIRCallNode"
  show (LIRCalloutNode label )                     = "LIRCalloutNode"
  show (LIREnterNode int)                          = "LIREnterNode" 
  show (LIRRetNode labels meth)                    = "LIRRetNode" 
  show (LIRIfNode expr label1 label2)              = "LIRIfNode"
  show (LIRJumpLabelNode label)                    = "LIRJumpLabelNode"
