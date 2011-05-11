{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}

module Decaf.SSANodes where
import Decaf.IR.SSA
import Decaf.IR.IRNode
import Data.Int
import Loligoptl
import qualified Loligoptl.Label

-- Graph
type SSAGraph = Graph SSANode

-- nodes
data SSANode e x where
  SSALabelNode        :: Label -> SSANode C O
  SSARegAssignNode    :: SSAReg -> SSAExpr -> SSANode O O
  SSARegOffAssignNode :: SSAReg -> SSAReg -> SSASize -> SSAOperand -> SSANode O O
  SSAStoreNode        :: SSAMemAddr -> SSAOperand -> SSANode O O
  SSALoadNode         :: SSAReg -> SSAMemAddr -> SSANode O O
  SSACallNode         :: Label -> Label -> SSANode O C -- method label and label for next line
  SSACalloutNode      :: String -> SSANode O O -- assume control falls through
  SSAEnterNode        :: SSAInt -> SSANode O O
  SSARetNode          :: [Label] -> String -> SSANode O C
  SSAIfNode           :: SSARelExpr -> Label -> Label -> SSANode O C  -- false, then true
  SSAJumpLabelNode    :: Label -> SSANode O C
    
instance NonLocal SSANode where
  entryLabel (SSALabelNode lab) = lab
  successors (SSARetNode labels _) = labels
  successors (SSACallNode l1 l2) = [l1, l2]
  successors (SSAJumpLabelNode lab) = [lab]
  successors (SSAIfNode expr flab tlab) = [flab, tlab]

ssaNodeToG :: forall e x. (ShapeLifter e x) => SSANode e x -> Graph SSANode e x
ssaNodeToG n = singletonG n

instance Show (SSANode e x) where
  show (SSALabelNode label )                       = pp $ SSALabelInst label
  show (SSARegAssignNode reg expr )                = pp $ SSARegAssignInst reg expr
  show (SSARegOffAssignNode reg reg' size operand) = "SSARegOffAssignNode"
  show (SSAStoreNode mem operand)                  = "SSAStoreNode"
  show (SSALoadNode reg mem)                       = "SSALoadNode" 
  show (SSACallNode label label')                  = "SSACallNode"
  show (SSACalloutNode label )                     = "SSACalloutNode"
  show (SSAEnterNode int)                          = "SSAEnterNode" 
  show (SSARetNode labels meth)                    = "SSARetNode" 
  show (SSAIfNode expr label1 label2)              = "SSAIfNode"
  show (SSAJumpLabelNode label)                    = "SSAJumpLabelNode"
