{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}

module Decaf.HooplNodes where
--import Compiler.Hoopl hiding (Top)
import Decaf.IR.LIR
import Loligoptl.Dataflow 
import Loligoptl.Graph 
import Loligoptl.Fuel 
import Loligoptl.Label 
import Data.Int

-- Labels 
type HooplLabel = Loligoptl.Label.Label

-- nodes
data Node e x where
  LIRLabelNode     :: HooplLabel -> Node C O

  LIRRegAssignNode :: LIRReg -> LIRExpr -> Node O O
  LIRTempEnterNode :: Int -> Node O O   -- 
  LIRRegOffAssignNode :: LIRReg -> LIRReg -> LIRSize -> LIROperand -> Node O O
  LIRStoreNode     :: LIRMemAddr -> LIROperand -> Node O O
  LIRLoadNode      :: LIRReg -> LIRMemAddr -> Node O O
  LIRCalloutNode   :: String -> Node O O -- assume control falls through
  LIREnterNode     :: Int64 -> Node O O

  LIRRetNode       :: Node O C
  LIRIfNode        :: LIRRelExpr -> HooplLabel -> HooplLabel -> Node O C  -- false, then true
  LIRJumpLabelNode :: HooplLabel -> Node O C
  LIRCallNode      :: [LIRLabel] -> Node O C -- list of labels includes method label and label for next line
    
instance NonLocal Node where
  entryLabel (LIRLabelNode lab) = lab
  successors (LIRRetNode) = []
  successors (LIRCallNode labs) = labs
  successors (LIRJumpLabelNode lab) = [lab]
  successors (LIRIfNode expr flab tlab) = [flab,tlab]

-- node to G
nodeToG :: forall e x. (ShapeLifter e x) => Node e x -> Graph Node e x
nodeToG n = singletonG n

-- join two change flag
joinChangeFlag :: ChangeFlag -> ChangeFlag -> ChangeFlag 
joinChangeFlag NoChange NoChange = NoChange
joinChangeFlag _ _               = SomeChange                             
