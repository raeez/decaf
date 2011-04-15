{-# LANGUAGE MultiParamTypeClasses
  , GADTs #-}

module Decaf.IR.ControlFlowGraph where
--import Decaf.IR.Class
import Data.Int
import Decaf.IR.IRNode
import Decaf.IR.AST
import Decaf.IR.LIR
import Decaf.IR.SymbolTable
import Decaf.Translator
import Decaf.HooplNodes
import Decaf.RegisterAllocator

import Loligoptl.Graph
import Loligoptl.Label 

import Control.Monad
import Data.Int

mkBasicBlock :: [LIRInst] -> ControlNode
mkBasicBlock = BasicBlock

-- converts CFGExprInsts into CFGLIRInsts
shortCircuit :: [CFGInst] -> Translator [CFGInst]
shortCircuit is = liftM concat $ mapM help is
  where 
    help :: CFGInst -> Translator [CFGInst]
    help inst = 
      case inst of
        CFGExprInst (CFGLogExpr expr1 op expr2 reg) ->
          do { contEvaling <- shortCircuit (expr2 : [CFGLIRInst $ LIRRegAssignInst reg (LIROperExpr (cgOper expr2))])

             ; expr1' <- shortCircuit [expr1]
             ; newif <- case op of
                          LAND -> makeIf (cgOper expr1) [shortCircuitInst asmFalse] contEvaling 
                          LOR ->  makeIf (cgOper expr1) contEvaling [shortCircuitInst asmTrue]
             ; return $ expr1' ++ newif }
          where shortCircuitInst b = CFGLIRInst $ LIRRegAssignInst reg (LIROperExpr (LIRIntOperand b))
        CFGExprInst (CFGFlatExpr is _) -> return is

        other -> return [other]

stripCFG :: [CFGInst] -> [LIRInst]
stripCFG is = map strip is
  where strip (CFGLIRInst inst) = inst
        strip _ = error "shortCircuit didn't work"

-- CHANGE FROM LIRInst to new itermediate ' type

pullOutLIR (CFGLIRInst x) = x

symToInt :: LIRReg -> Int
symToInt (SREG s) = s
symToInt  reg = error "attempted to label if using non-symbolic register"
--symToInt (LIRIntOperand x) = error "attempted to label if using int literal"

-- | Main function.  Takes HIR and turns it into Hoopl graph
graphProgram :: Int -> SymbolTree -> DecafProgram -> DecafGraph C C
graphProgram rc st dp =
  let g (CFGUnit lab insts) = (CFGLIRInst $ LIRLabelInst lab) : insts 
      (res, _) = runTranslator (do prog <- translateProgram st dp
                                   mapM (shortCircuit.g) (cgProgUnits prog) 
                                          >>= (return . (map stripCFG)))
                               (mkNamespace rc)
      resProg = LIRProgram (LIRLabel "" 0) (map (LIRUnit (LIRLabel "" 0)) res) 
      (res', _, _) = allocateRegisters st resProg
      instructions = (concat . (map lirUnitInstructions) . lirProgUnits) resProg
      blocks = makeBlocks instructions
        where
          -- inserts labels for function jumps

  in GMany NothingO (mapFromList $ zip (map entryLabel blocks) blocks)  NothingO -- should change probably

makeBlocks :: [LIRInst] -> [Block Node C C]
makeBlocks insts = startBlock [] insts
  where
    startBlock  :: [Block Node C C] -> [LIRInst] -> [Block Node C C]
    startBlock bs [] = bs
    startBlock bs (inst:is) = 
      case inst of
        -- Closed Open nodes
        LIRLabelInst lab -> buildBlock bs (BFirst $ LIRLabelNode $ lab) is
        other -> startBlock bs is -- skip the node; it's dead code (hopefully...)

    buildBlock :: [Block Node C C] -> Block Node C O -> [LIRInst] -> [Block Node C C]
    buildBlock bs b [] = error "unfinished block"
    buildBlock bs b (inst:is) = 
      case inst of
        -- Open Closed nodes
        LIRRetInst -> 
          startBlock ((b `BClosed` (BLast LIRRetNode)) : bs) is
        LIRIfInst expr falselabel truelabel -> 
          startBlock ((b `BClosed` (BLast $ LIRIfNode expr falselabel truelabel)) : bs) is
        LIRCallInst lab ->
          startBlock ((b `BClosed` (BLast $ LIRCallNode lab Nothing)) : bs) is -- add another label hopefully
        LIRJumpLabelInst lab ->
          startBlock ((b `BClosed` (BLast $ LIRJumpLabelNode lab)) : bs) is

        -- Open Open nodes
        LIRCalloutInst name -> -- this one is borderline...
          buildBlock bs (b `BCat` (BMiddle $ LIRCalloutNode name)) is
        LIRRegAssignInst reg expr ->
          buildBlock bs (b `BCat` (BMiddle $ LIRRegAssignNode reg expr)) is
        LIRRegOffAssignInst reg reg' size oper ->
          buildBlock bs (b `BCat` (BMiddle $ LIRRegOffAssignNode reg reg' size oper)) is
        LIRStoreInst mem oper ->
          buildBlock bs (b `BCat` (BMiddle $ LIRStoreNode mem oper)) is
        LIRLoadInst reg mem ->
          buildBlock bs (b `BCat` (BMiddle $ LIRLoadNode reg mem)) is
        LIREnterInst int ->
          buildBlock bs (b `BCat` (BMiddle $ LIREnterNode int)) is

--      LIRRegCmpAssignInst LIRReg LIRExpr LIRLabel  -- needed?
--    where toLabel = Label . uniqueID -- not needed hopefully

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
nodeToLIR (LIRLabelNode l)                = [LIRLabelInst l]
nodeToLIR (LIRRegAssignNode r e)          = [LIRRegAssignInst r e]
nodeToLIR (LIRRegOffAssignNode r1 r2 s o) = [LIRRegOffAssignInst r1 r2 s o]
nodeToLIR (LIREnterNode i)                = [LIREnterInst i]
nodeToLIR (LIRIfNode e fl tl)             = [LIRIfInst e fl tl]
nodeToLIR (LIRJumpLabelNode l)            = [LIRJumpLabelInst l]
nodeToLIR (LIREnterNode i)                = [LIREnterInst i]
nodeToLIR (LIRStoreNode m o)              = [LIRStoreInst m o]
nodeToLIR (LIRLoadNode r m)               = [LIRLoadInst r m]
nodeToLIR (LIRCalloutNode s)              = [LIRCalloutInst s]
nodeToLIR (LIRJumpLabelNode l)            = [LIRJumpLabelInst l]
nodeToLIR (LIRCallNode l _)               = [LIRCallInst l]
