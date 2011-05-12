{-# LANGUAGE GADTs, MultiParamTypeClasses, RankNTypes #-}

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

import Debug.Trace

-- Graph
type ASMGraph = Graph ASMNode'

-- Labels 
type HooplLabel = Loligoptl.Label.Label

-- nodes
data ASMNode' e x where
    ASMLabelNode' :: Int -> ASMLabel -> ASMNode' C O

    ASMAddNode'   :: Int -> ASMOperand -> ASMOperand -> ASMNode' O O
    ASMSubNode'   :: Int -> ASMOperand -> ASMOperand -> ASMNode' O O
    ASMMovNode'   :: Int -> ASMOperand -> ASMOperand -> ASMNode' O O
    ASMCmpNode'   :: Int -> ASMOperand -> ASMOperand -> ASMNode' O O
    ASMAndNode'   :: Int -> ASMOperand -> ASMOperand -> ASMNode' O O
    ASMOrNode'    :: Int -> ASMOperand -> ASMOperand -> ASMNode' O O 
    ASMXorNode'   :: Int -> ASMOperand -> ASMOperand -> ASMNode' O O
    ASMShlNode'   :: Int -> ASMOperand -> ASMOperand -> ASMNode' O O
    ASMShrNode'   :: Int -> ASMOperand -> ASMOperand -> ASMNode' O O
    ASMShraNode'  :: Int -> ASMOperand -> ASMOperand -> ASMNode' O O
    ASMMulNode'   :: Int -> ASMOperand -> ASMNode' O O
    ASMDivNode'   :: Int -> ASMOperand -> ASMNode' O O
    ASMModNode'   :: Int -> ASMOperand -> ASMNode' O O
    ASMPushNode'  :: Int -> ASMOperand -> ASMNode' O O
    ASMPopNode'   :: Int -> ASMOperand -> ASMNode' O O
    ASMNegNode'   :: Int -> ASMOperand -> ASMNode' O O
    ASMNotNode'   :: Int -> ASMOperand -> ASMNode' O O
    ASMJmpNode'   :: Int -> ASMLabel -> ASMLabel -> ASMNode' O C
    ASMJeNode'    :: Int -> ASMLabel -> ASMLabel -> ASMNode' O C
    ASMJneNode'   :: Int -> ASMLabel -> ASMLabel -> ASMNode' O C
    ASMJgNode'    :: Int -> ASMLabel -> ASMLabel -> ASMNode' O C
    ASMJgeNode'   :: Int -> ASMLabel -> ASMLabel -> ASMNode' O C
    ASMJlNode'    :: Int -> ASMLabel -> ASMLabel -> ASMNode' O C
    ASMJleNode'   :: Int -> ASMLabel -> ASMLabel -> ASMNode' O C

    ASMCallNode'  :: Int -> ASMSym -> ASMNode' O C
    ASMEnterNode' :: Int -> ASMInt -> ASMNode' O O
    ASMRetNode'   :: Int -> ASMNode' O C

instance Show (ASMNode' e x) where
  show n = show $ asmDropPrime n

data ASMNode where
    ASMAddNode   :: Int -> ASMOperand -> ASMOperand -> ASMNode  
    ASMSubNode   :: Int -> ASMOperand -> ASMOperand -> ASMNode  
    ASMMovNode   :: Int -> ASMOperand -> ASMOperand -> ASMNode  
    ASMCmpNode   :: Int -> ASMOperand -> ASMOperand -> ASMNode  
    ASMAndNode   :: Int -> ASMOperand -> ASMOperand -> ASMNode  
    ASMOrNode    :: Int -> ASMOperand -> ASMOperand -> ASMNode   
    ASMXorNode   :: Int -> ASMOperand -> ASMOperand -> ASMNode  
    ASMShlNode   :: Int -> ASMOperand -> ASMOperand -> ASMNode  
    ASMShrNode   :: Int -> ASMOperand -> ASMOperand -> ASMNode  
    ASMShraNode  :: Int -> ASMOperand -> ASMOperand -> ASMNode  
    ASMMulNode   :: Int -> ASMOperand -> ASMNode  
    ASMDivNode   :: Int -> ASMOperand -> ASMNode  
    ASMModNode   :: Int -> ASMOperand -> ASMNode  
    ASMPushNode  :: Int -> ASMOperand -> ASMNode  
    ASMPopNode   :: Int -> ASMOperand -> ASMNode  
    ASMNegNode   :: Int -> ASMOperand -> ASMNode  
    ASMNotNode   :: Int -> ASMOperand -> ASMNode  
    ASMJmpNode   :: Int -> ASMLabel -> ASMNode  
    ASMJeNode    :: Int -> ASMLabel -> ASMNode  
    ASMJneNode   :: Int -> ASMLabel -> ASMNode  
    ASMJgNode    :: Int -> ASMLabel -> ASMNode  
    ASMJgeNode   :: Int -> ASMLabel -> ASMNode  
    ASMJlNode    :: Int -> ASMLabel -> ASMNode  
    ASMJleNode   :: Int -> ASMLabel -> ASMNode  
    ASMLabelNode :: Int -> ASMLabel -> ASMNode
    ASMCallNode  :: Int -> ASMSym -> ASMNode  
    ASMEnterNode :: Int -> ASMInt -> ASMNode  
    ASMRetNode   :: Int -> ASMNode  
                     deriving (Show, Eq, Ord)

-- should be generic...

{-asmAddPrime :: forall e x. ASMNode -> ASMNode' e x
asmAddPrime n = 
  case n of
    (ASMAddNode i op1 op2) -> (ASMAddNode' i op1 op2)
    (ASMSubNode i op1 op2) -> (ASMSubNode' i op1 op2)
    (ASMMovNode i op1 op2) -> (ASMMovNode' i op1 op2)
    (ASMCmpNode i op1 op2) -> (ASMCmpNode' i op1 op2)
    (ASMAndNode i op1 op2) -> (ASMAndNode' i op1 op2)
    (ASMOrNode  i op1 op2) -> (ASMOrNode' i op1 op2)
    (ASMXorNode i op1 op2) -> (ASMXorNode' i op1 op2)
    (ASMShlNode i op1 op2) -> (ASMShlNode' i op1 op2)
    (ASMShrNode i op1 op2) -> (ASMShrNode' i op1 op2)
    (ASMShraNode i op1 op2) -> (ASMShraNode' i op1 op2)
    (ASMPushNode i op) -> (ASMPushNode' i op)
    (ASMPopNode  i op) -> (ASMPopNode' i op)
    (ASMNegNode  i op) -> (ASMNegNode' i op)
    (ASMNotNode  i op) -> (ASMNotNode' i op)
    (ASMMulNode i op) -> (ASMMulNode' i op)
    (ASMDivNode i op) -> (ASMDivNode' i op)
    (ASMModNode i op) -> (ASMModNode' i op)
    (ASMJmpNode i o  ) -> (ASMJmpNode' i o)
    (ASMJeNode  i o  ) -> (ASMJeNode' i o)
    (ASMJneNode i o  ) -> (ASMJneNode' i o)
    (ASMJgNode  i o  ) -> (ASMJgNode' i o)
    (ASMJgeNode i o  ) -> (ASMJgeNode' i o)
    (ASMJlNode  i o  ) -> (ASMJlNode' i o)
    (ASMJleNode i o  ) -> (ASMJleNode' i o)
    (ASMLabelNode i o  ) -> (ASMLabelNode' i o)
    (ASMCallNode i o  ) -> (ASMCallNode' i o)
    (ASMEnterNode i o  ) -> (ASMEnterNode' i o)
    (ASMRetNode i  ) -> (ASMRetNode' i)
-}
asmDropPrime :: ASMNode' e x -> ASMNode
asmDropPrime n = 
  case n of
    (ASMLabelNode' i o  ) -> (ASMLabelNode i o)

    (ASMAddNode' i op1 op2) -> (ASMAddNode i op1 op2)
    (ASMSubNode' i op1 op2) -> (ASMSubNode i op1 op2)
    (ASMMovNode' i op1 op2) -> (ASMMovNode i op1 op2)
    (ASMCmpNode' i op1 op2) -> (ASMCmpNode i op1 op2)
    (ASMAndNode' i op1 op2) -> (ASMAndNode i op1 op2)
    (ASMOrNode'  i op1 op2) -> (ASMOrNode i op1 op2)
    (ASMXorNode' i op1 op2) -> (ASMXorNode i op1 op2)
    (ASMShlNode' i op1 op2) -> (ASMShlNode i op1 op2)
    (ASMShrNode' i op1 op2) -> (ASMShrNode i op1 op2)
    (ASMShraNode' i op1 op2) -> (ASMShraNode i op1 op2)
    (ASMPushNode' i op) -> (ASMPushNode i op)
    (ASMPopNode'  i op) -> (ASMPopNode i op)
    (ASMNegNode'  i op) -> (ASMNegNode i op)
    (ASMNotNode'  i op) -> (ASMNotNode i op)
    (ASMMulNode' i op) -> (ASMMulNode i op)
    (ASMDivNode' i op) -> (ASMDivNode i op)
    (ASMModNode' i op) -> (ASMModNode i op)
    (ASMEnterNode' i o  ) -> (ASMEnterNode i o)

    (ASMJmpNode' i o o') -> (ASMJmpNode i o)
    (ASMJeNode'  i o o') -> (ASMJeNode i o)
    (ASMJneNode' i o o') -> (ASMJneNode i o)
    (ASMJgNode'  i o o') -> (ASMJgNode i o)
    (ASMJgeNode' i o o') -> (ASMJgeNode i o)
    (ASMJlNode'  i o o') -> (ASMJlNode i o)
    (ASMJleNode' i o o') -> (ASMJleNode i o)
    (ASMCallNode' i o  ) -> (ASMCallNode i o)
    (ASMRetNode' i  ) -> (ASMRetNode i)


getASMLine n = case n of            
      (ASMLabelNode i _  ) -> i

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

      (ASMJmpNode   i _  ) -> i
      (ASMJeNode    i _  ) -> i
      (ASMJneNode   i _  ) -> i
      (ASMJgNode    i _  ) -> i
      (ASMJgeNode   i _  ) -> i
      (ASMJlNode    i _  ) -> i
      (ASMJleNode   i _  ) -> i
      (ASMCallNode  i _  ) -> i
      (ASMRetNode   i    ) -> i



{-class HasLine e x where
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

-}        

    
instance NonLocal ASMNode' where
  entryLabel (ASMLabelNode' _ l) =  asmLabeltoHoopl l -- hoopllabel = lirlabel
  successors (ASMJmpNode' _ l l') = [asmLabeltoHoopl l, asmLabeltoHoopl l']
  successors (ASMJeNode' _ l l')  = [asmLabeltoHoopl l, asmLabeltoHoopl l']
  successors (ASMJneNode' _ l l') = [asmLabeltoHoopl l, asmLabeltoHoopl l']
  successors (ASMJgNode' _ l l')  = [asmLabeltoHoopl l, asmLabeltoHoopl l']
  successors (ASMJgeNode' _ l l') = [asmLabeltoHoopl l, asmLabeltoHoopl l']
  successors (ASMJlNode' _ l l')  = [asmLabeltoHoopl l, asmLabeltoHoopl l']
  successors (ASMJleNode' _ l l') = [asmLabeltoHoopl l, asmLabeltoHoopl l']

asmLabeltoHoopl (ASMLabel l i ) = LIRLabel l i


-- Turns numbered ASMInst list into ASMNode' Graph
graphASMList :: [(ASMInst, Int)] -> Graph ASMNode' C C
graphASMList prog =
  let blocks = makeBlocks prog
  in GMany NothingO (mapFromList (zip (map entryLabel blocks) blocks)) NothingO

makeBlocks :: [(ASMInst, Int)] -> [Block ASMNode' C C]
makeBlocks insts = startBlock [] insts
  where
    startBlock  :: [Block ASMNode' C C] -> [(ASMInst, Int)] -> [Block ASMNode' C C]
    startBlock bs [] = bs
    startBlock bs ((inst, i):is) = 
      case inst of
        -- Closed Open nodes
        ASMLabelInst lab -> (buildBlock bs (BFirst $ ASMLabelNode' i lab) is)
        other -> (startBlock bs is) -- skip the node; it's dead code (hopefully...)

    buildBlock :: [Block ASMNode' C C] -> Block ASMNode' C O -> [(ASMInst, Int)] -> [Block ASMNode' C C]
--    buildBlock bs b [] = error ("unfinished block: " ++ (show b))
    buildBlocks bs b [] = bs -- drop the unfinished block
    buildBlock bs b ((inst,i):is) = 
      case inst of
        ASMAddInst  op1 op2 -> buildBlock bs (b `BHead` (ASMAddNode' i op1 op2)) is
        ASMSubInst  op1 op2 -> buildBlock bs (b `BHead` (ASMSubNode' i op1 op2)) is
        ASMMovInst  op1 op2 -> buildBlock bs (b `BHead` (ASMMovNode' i op1 op2)) is
        ASMCmpInst  op1 op2 -> buildBlock bs (b `BHead` (ASMCmpNode' i op1 op2)) is
        ASMAndInst  op1 op2 -> buildBlock bs (b `BHead` (ASMAndNode' i op1 op2)) is
        ASMOrInst   op1 op2 -> buildBlock bs (b `BHead` (ASMOrNode'  i op1 op2)) is
        ASMXorInst  op1 op2 -> buildBlock bs (b `BHead` (ASMXorNode' i op1 op2)) is
        ASMShlInst  op1 op2 -> buildBlock bs (b `BHead` (ASMShlNode' i op1 op2)) is
        ASMShrInst  op1 op2 -> buildBlock bs (b `BHead` (ASMShrNode' i op1 op2)) is
        ASMShraInst op1 op2 -> buildBlock bs (b `BHead` (ASMShraNode' i op1 op2)) is

        ASMMulInst  op -> buildBlock bs (b `BHead` (ASMMulNode' i op)) is
        ASMDivInst  op -> buildBlock bs (b `BHead` (ASMDivNode' i op)) is
        ASMModInst  op -> buildBlock bs (b `BHead` (ASMModNode' i op)) is
        ASMPushInst op -> buildBlock bs (b `BHead` (ASMPushNode' i op)) is
        ASMPopInst  op -> buildBlock bs (b `BHead` (ASMPopNode' i op)) is
        ASMNegInst  op -> buildBlock bs (b `BHead` (ASMNegNode' i op)) is
        ASMNotInst  op -> buildBlock bs (b `BHead` (ASMNotNode' i op)) is

        ASMEnterInst int -> buildBlock bs (b `BHead` (ASMEnterNode' i int)) is


--         ASMJmpInst lab -> startBlock ((b `BClosed` (BLast $ ASMJmpNode' i lab)) : bs) is
--         ASMJeInst  lab -> startBlock ((b `BClosed` (BLast $ ASMJeNode'  i lab)) : bs) is
--         ASMJneInst lab -> startBlock ((b `BClosed` (BLast $ ASMJneNode' i lab)) : bs) is
--         ASMJgInst  lab -> startBlock ((b `BClosed` (BLast $ ASMJgNode'  i lab)) : bs) is
--         ASMJgeInst lab -> startBlock ((b `BClosed` (BLast $ ASMJgeNode' i lab)) : bs) is
--         ASMJlInst  lab -> startBlock ((b `BClosed` (BLast $ ASMJlNode'  i lab)) : bs) is
--         ASMJleInst lab -> startBlock ((b `BClosed` (BLast $ ASMJleNode' i lab)) : bs) is
--         ASMCallInst sym -> startBlock ((b `BClosed` (BLast $ ASMCallNode' i sym)) : bs) is
--         ASMRetInst     -> startBlock ((b `BClosed` (BLast $ ASMRetNode' i)) : bs) is 
        ASMJmpInst lab -> processJump ASMJmpNode' lab
        ASMJeInst  lab -> processJump ASMJeNode'  lab
        ASMJneInst lab -> processJump ASMJneNode' lab
        ASMJgInst  lab -> processJump ASMJgNode'  lab
        ASMJgeInst lab -> processJump ASMJgeNode' lab
        ASMJlInst  lab -> processJump ASMJlNode'  lab
        ASMJleInst lab -> processJump ASMJleNode' lab
        ASMCallInst sym -> startBlock ((b `BClosed` (BLast $ ASMCallNode' i sym)) : bs) is
        ASMRetInst     -> startBlock ((b `BClosed` (BLast $ ASMRetNode' i)) : bs) is 
        other -> error ("bad inst: " ++ show other ++ "\n" ++ unlines (map show bs) ++ show b) --buildBlock bs b is -- skip redundant label

      where newlab = (ASMLabel "imagine" (-i)) -- does the minus thing work??
            processJump :: (Int -> ASMLabel -> ASMLabel -> ASMNode' O C) 
                        -> ASMLabel -> [Block ASMNode' C C]
            processJump const lab = 
              case is of -- if next inst is a label, fall through to it, otherwise make new label
                ((ASMLabelInst nextlab),j):xs -> 
                  startBlock ((b `BClosed` (BLast $ const i lab nextlab)) : bs) is
                otherwise ->
                  startBlock ((b `BClosed` (BLast $ const i lab newlab)) : bs)
                               (((ASMLabelInst newlab), (-i)) : is)

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
