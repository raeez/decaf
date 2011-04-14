{-# LANGUAGE GADTs #-}

module Transform where
import Loligoptl.Label
import Loligoptl.Graph
import Decaf.IR.LIR
import Decaf.HooplNodes

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
nodeToLIR (LIRRegAssignNode r e)          = [LIRRegAssignInst r e]
nodeToLIR (LIRRegOffAssignNode r1 r2 s o) = [LIRRegOffAssignInst r1 r2 s o]
nodeToLIR (LIRIfNode e fl tl)             = [LIRIfInst e fl tl]
nodeToLIR (LIRJumpLabelNode l)            = [LIRJumpLabelInst (lirLabel l)]
nodeToLIR (LIRTempEnterNode i)            = [LIREnterInst i]
nodeToLIR (LIRLoadNode r m)               = [LIRLoadInst r m]
nodeToLIR (LIRStoreNode m o)              = [LIRStoreInst m o]

-- lirLabel :: Label -> LIRLabel
-- lirLabel (Label n) = LIRLabel "" n
lirLabel n = LIRLabel "" 0
