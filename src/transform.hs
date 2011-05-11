{-# LANGUAGE GADTs #-}

module Transform where
import Loligoptl.Label
import Loligoptl.Graph
import Decaf.IR.LIR
import Decaf.LIRNodes

graphToLIR :: Graph' Block Node e x -> [LIRInst]
graphToLIR (GNil) = []
graphToLIR (GUnit unit) = blockToLIR unit
graphToLIR (GMany entry labels exit) = mapEntry ++ mapExit
  where
    mapEntry = case entry of
                  JustO v  -> blockToLIR v
                  NothingO -> []
    mapExit = case exit of
                  JustO v  -> blockToLIR v
                  NothingO -> []

blockToLIR :: Block Node e x -> [LIRInst]
blockToLIR (BFirst node)   = nodeToLIR node
blockToLIR (BMiddle node)  = nodeToLIR node
blockToLIR (BLast node)    = nodeToLIR node
blockToLIR (BCat b1 b2)    = blockToLIR b1 ++ blockToLIR b2
blockToLIR (BHead b n)     = blockToLIR b  ++ nodeToLIR n
blockToLIR (BTail n b)     = nodeToLIR n   ++ blockToLIR b
blockToLIR (BClosed b1 b2) = blockToLIR b1 ++ blockToLIR b2

nodeToLIR :: Node e x -> [LIRInst]
nodeToLIR (LIRLabelNode l)                = [LIRLabel l]
nodeToLIR (LIRRegAssignNode r e)          = [LIRRegAssignInst r e]
nodeToLIR (LIRRegOffAssignNode r1 r2 s o) = [LIRRegOffAssignInst r1 r2 s o]
nodeToLIR (LIREnterNode i)                = [LIREnterNode i]
nodeToLIR (LIRIfNode e fl tl)             = [LIRIfInst e fl tl]
nodeToLIR (LIRJumpLabelNode l)            = [LIRJumpLabelInst l]
nodeToLIR (LIREnterNode i)                = [LIREnterInst i]
nodeToLIR (LIRStoreNode m o)              = [LIRStoreInst m o]
nodeToLIR (LIRLoadNode r m)               = [LIRLoadInst r m]
nodeToLIR (LIRCalloutNode s)              = [LIRCalloutInst s]
nodeToLIR (LIRJumpLabelNode l)            = [LIRJumpLabelInst l]
nodeToLIR (LIRCallNode l)                 = [LIRCallInst l]
