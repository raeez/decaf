{-# LANGUAGE GADTs, MultiParamTypeClasses #-}

{- 
possible bugs:
not every OC node is followed by a CO label node
-}
module Decaf.RegisterAllocator.GraphASM where
--import Compiler.Hoopl hiding (Top)
import Decaf.IR.LIR
import Decaf.IR.IRNode
import Decaf.IR.ASM
import Loligoptl
import Loligoptl.Dataflow 
import Loligoptl.Graph 
import Loligoptl.Fuel 
import Loligoptl.Label 
import Data.Int

-- Graph
type ASMGraph = Graph ASMNode

-- Labels 
type HooplLabel = Loligoptl.Label.Label

-- nodes
data ASMNode e x where
    ASMAddNode   :: Int -> ASMOperand -> ASMOperand -> ASMNode O O
    ASMSubNode   :: Int -> ASMOperand -> ASMOperand -> ASMNode O O
    ASMMovNode   :: Int -> ASMOperand -> ASMOperand -> ASMNode O O
    ASMCmpNode   :: Int -> ASMOperand -> ASMOperand -> ASMNode O O
    ASMAndNode   :: Int -> ASMOperand -> ASMOperand -> ASMNode O O
    ASMOrNode    :: Int -> ASMOperand -> ASMOperand -> ASMNode O O 
    ASMXorNode   :: Int -> ASMOperand -> ASMOperand -> ASMNode O O
    ASMShlNode   :: Int -> ASMOperand -> ASMOperand -> ASMNode O O
    ASMShrNode   :: Int -> ASMOperand -> ASMOperand -> ASMNode O O
    ASMShraNode  :: Int -> ASMOperand -> ASMOperand -> ASMNode O O
    ASMMulNode   :: Int -> ASMOperand -> ASMNode O O
    ASMDivNode   :: Int -> ASMOperand -> ASMNode O O
    ASMModNode   :: Int -> ASMOperand -> ASMNode O O
    ASMPushNode  :: Int -> ASMOperand -> ASMNode O O
    ASMPopNode   :: Int -> ASMOperand -> ASMNode O O
    ASMNegNode   :: Int -> ASMOperand -> ASMNode O O
    ASMNotNode   :: Int -> ASMOperand -> ASMNode O O
    ASMJmpNode   :: Int -> ASMLabel -> ASMNode O C
    ASMJeNode    :: Int -> ASMLabel -> ASMNode O C
    ASMJneNode   :: Int -> ASMLabel -> ASMNode O C
    ASMJgNode    :: Int -> ASMLabel -> ASMNode O C
    ASMJgeNode   :: Int -> ASMLabel -> ASMNode O C
    ASMJlNode    :: Int -> ASMLabel -> ASMNode O C
    ASMJleNode   :: Int -> ASMLabel -> ASMNode O C
    ASMLabelNode :: Int -> ASMLabel -> ASMNode C O
    ASMCallNode  :: Int -> ASMSym -> ASMNode O C
    ASMEnterNode :: Int -> ASMInt -> ASMNode O O
    ASMRetNode   :: Int -> ASMNode O C

class HasLine e x where
  getASMLine :: ASMNode e x -> Int

instance HasLine C O where
  getASMLine n = 
    case n of
      (ASMLabelNode i _  ) -> i

instance HasLine O O where
  getASMLine n = 
    case n of
      (ASMAddNode   i _ _) -> i
      (ASMSubNode   i _ _) -> i
      (ASMMovNode   i _ _) -> i
      (ASMCmpNode   i _ _) -> i
      (ASMAndNode   i _ _) -> i
      (ASMOrNode    i _ _) -> i
      (ASMXorNode   i _ _) -> i
      (ASMShlNode   i _ _) -> i
      (ASMShrNode   i _ _) -> i
      (ASMShraNode  i _ _) -> i
      (ASMMulNode   i _  ) -> i
      (ASMDivNode   i _  ) -> i
      (ASMModNode   i _  ) -> i
      (ASMPushNode  i _  ) -> i
      (ASMPopNode   i _  ) -> i
      (ASMNegNode   i _  ) -> i
      (ASMNotNode   i _  ) -> i
      (ASMEnterNode i _  ) -> i

instance HasLine O C where
  getASMLine n = 
    case n of
      (ASMJmpNode   i _  ) -> i
      (ASMJeNode    i _  ) -> i
      (ASMJneNode   i _  ) -> i
      (ASMJgNode    i _  ) -> i
      (ASMJgeNode   i _  ) -> i
      (ASMJlNode    i _  ) -> i
      (ASMJleNode   i _  ) -> i

      (ASMCallNode  i _  ) -> i
      (ASMRetNode   i    ) -> i

        

    
instance NonLocal ASMNode where
  entryLabel (ASMLabelNode _ l) =  asmtohooplLabel l -- hoopllabel = lirlabel
  successors (ASMJmpNode _ l) = [asmtohooplLabel l]
  successors (ASMJeNode _ l)  = [asmtohooplLabel l]
  successors (ASMJneNode _ l) = [asmtohooplLabel l]
  successors (ASMJgNode _ l)  = [asmtohooplLabel l]
  successors (ASMJgeNode _ l) = [asmtohooplLabel l]
  successors (ASMJlNode _ l)  = [asmtohooplLabel l]
  successors (ASMJleNode _ l) = [asmtohooplLabel l]

asmtohooplLabel (ASMLabel l i ) = LIRLabel l i


-- Turns numbered ASMInst list into ASMNode Graph
graphASMList :: [(ASMInst, Int)] -> Graph ASMNode C C
graphASMList prog =
  let blocks = makeBlocks prog
  in GMany NothingO (mapFromList (zip (map entryLabel blocks) blocks)) NothingO

makeBlocks :: [(ASMInst, Int)] -> [Block ASMNode C C]
makeBlocks insts = startBlock [] insts
  where
    startBlock  :: [Block ASMNode C C] -> [(ASMInst, Int)] -> [Block ASMNode C C]
    startBlock bs [] = bs
    startBlock bs ((inst, i):is) = 
      case inst of
        -- Closed Open nodes
        ASMLabelInst lab -> (buildBlock bs (BFirst $ ASMLabelNode i lab) is)
        other -> (startBlock bs is) -- skip the node; it's dead code (hopefully...)

    buildBlock :: [Block ASMNode C C] -> Block ASMNode C O -> [(ASMInst, Int)] -> [Block ASMNode C C]
    buildBlock bs b [] = error "unfinished block"
    buildBlock bs b ((inst,i):is) = 
      case inst of
        ASMAddInst  op1 op2 -> buildBlock bs (b `BHead` (ASMAddNode i op1 op2)) is
        ASMSubInst  op1 op2 -> buildBlock bs (b `BHead` (ASMSubNode i op1 op2)) is
        ASMMovInst  op1 op2 -> buildBlock bs (b `BHead` (ASMMovNode i op1 op2)) is
        ASMCmpInst  op1 op2 -> buildBlock bs (b `BHead` (ASMCmpNode i op1 op2)) is
        ASMAndInst  op1 op2 -> buildBlock bs (b `BHead` (ASMAndNode i op1 op2)) is
        ASMOrInst   op1 op2 -> buildBlock bs (b `BHead` (ASMOrNode  i op1 op2)) is
        ASMXorInst  op1 op2 -> buildBlock bs (b `BHead` (ASMXorNode i op1 op2)) is
        ASMShlInst  op1 op2 -> buildBlock bs (b `BHead` (ASMShlNode i op1 op2)) is
        ASMShrInst  op1 op2 -> buildBlock bs (b `BHead` (ASMShrNode i op1 op2)) is
        ASMShraInst op1 op2 -> buildBlock bs (b `BHead` (ASMShraNode i op1 op2)) is

        ASMMulInst  op -> buildBlock bs (b `BHead` (ASMMulNode i op)) is
        ASMDivInst  op -> buildBlock bs (b `BHead` (ASMDivNode i op)) is
        ASMModInst  op -> buildBlock bs (b `BHead` (ASMModNode i op)) is
        ASMPushInst op -> buildBlock bs (b `BHead` (ASMPushNode i op)) is
        ASMPopInst  op -> buildBlock bs (b `BHead` (ASMPopNode i op)) is
        ASMNegInst  op -> buildBlock bs (b `BHead` (ASMNegNode i op)) is
        ASMNotInst  op -> buildBlock bs (b `BHead` (ASMNotNode i op)) is

        ASMJmpInst lab -> startBlock ((b `BClosed` (BLast $ ASMJmpNode i lab)) : bs) is
        ASMJeInst  lab -> startBlock ((b `BClosed` (BLast $ ASMJeNode  i lab)) : bs) is
        ASMJneInst lab -> startBlock ((b `BClosed` (BLast $ ASMJneNode i lab)) : bs) is
        ASMJgInst  lab -> startBlock ((b `BClosed` (BLast $ ASMJgNode  i lab)) : bs) is
        ASMJgeInst lab -> startBlock ((b `BClosed` (BLast $ ASMJgeNode i lab)) : bs) is
        ASMJlInst  lab -> startBlock ((b `BClosed` (BLast $ ASMJlNode  i lab)) : bs) is
        ASMJleInst lab -> startBlock ((b `BClosed` (BLast $ ASMJleNode i lab)) : bs) is

-- graphToLIR :: Graph' Block Node e x -> [LIRInst]
-- graphToLIR (GNil) =  []
-- graphToLIR (GUnit unit) = blockToLIR unit
-- graphToLIR (GMany entry labels exit) = (mapEntry ++ mapLabel ++ mapExit)
--   where
--     mapEntry :: [LIRInst]
--     mapEntry = case entry of
--                   JustO v  -> blockToLIR v
--                   NothingO -> []
--     mapExit :: [LIRInst]
--     mapExit = case exit of
--                   JustO v  -> blockToLIR v
--                   NothingO -> []
--     mapLabel = concatMap (blockToLIR.snd) (mapToList labels)

-- blockToLIR :: Block Node e x -> [LIRInst]
-- blockToLIR (BFirst node)   = nodeToLIR node
-- blockToLIR (BMiddle node)  = nodeToLIR node
-- blockToLIR (BLast node)    = nodeToLIR node
-- blockToLIR (BCat b1 b2)    = blockToLIR b1 ++ blockToLIR b2
-- blockToLIR (BHead b n)     = blockToLIR b  ++ nodeToLIR n
-- blockToLIR (BTail n b)     = nodeToLIR n   ++ blockToLIR b
-- blockToLIR (BClosed b1 b2) = blockToLIR b1 ++ blockToLIR b2

-- nodeToLIR :: Node e x -> [LIRInst]
-- nodeToLIR (LIRLabelNode l)                = [LIRLabelInst l]
-- nodeToLIR (LIRRegAssignNode r e)          = [LIRRegAssignInst r e]
-- nodeToLIR (LIRRegOffAssignNode r1 r2 s o) = [LIRRegOffAssignInst r1 r2 s o]
-- nodeToLIR (LIRStoreNode m o)              = [LIRStoreInst m o]
-- nodeToLIR (LIRLoadNode r m)               = [LIRLoadInst r m]
-- nodeToLIR (LIRCalloutNode s)              = [LIRCalloutInst s]
-- nodeToLIR (LIREnterNode i)                = [LIREnterInst i]
-- nodeToLIR (LIRRetNode l m)                = [LIRRetInst l m]
-- nodeToLIR (LIRIfNode e fl tl)             = [LIRIfInst e fl tl]
-- nodeToLIR (LIRJumpLabelNode l)            = [LIRJumpLabelInst l]
-- nodeToLIR (LIRCallNode l1 l2)             = [LIRCallInst l1 l2]
