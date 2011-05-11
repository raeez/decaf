{-# LANGUAGE MultiParamTypeClasses
  , GADTs #-}

module Decaf.IR.ControlFlowGraph where
import Data.Int
import qualified Data.Map as Map
import Decaf.IR.IRNode
import Decaf.IR.AST
import Decaf.IR.LIR
import Decaf.IR.SymbolTable
import Decaf.LIRTranslator
import Decaf.LIRNodes
import Decaf.RegisterAllocator
import System.IO.Unsafe
import Loligoptl
import Debug.Trace
import Control.Monad
import Data.Int

mkBasicBlock :: [LIRInst] -> ControlNode
mkBasicBlock = BasicBlock

-- converts CFGExprInsts into CFGLIRInsts
shortCircuit :: [CFGInst] -> LIRTranslator [CFGInst]
shortCircuit is = liftM concat $ mapM help is
  where 
    help :: CFGInst -> LIRTranslator [CFGInst]
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
        CFGExprInst (CFGFlatExpr is _) -> shortCircuit is

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
graphProgram :: SymbolTree -> DecafProgram -> Int -> LIRGraph C C
graphProgram st dp rc =
    let g (CFGUnit lab insts) = (CFGLIRInst $ LIRLabelInst lab) : insts 
        (res, Namespace _ _ _ _ _ sm) = -- grab the successor map
            runLIR (do prog <- translateProgram st dp
                       mapM (shortCircuit.g) (cgProgUnits prog) 
                                     >>= (return . (map stripCFG)))
                       (mkNamespace rc)
        resProg = LIRProgram (LIRLabel "" 0) (map (LIRUnit (LIRLabel "" 0)) res) 
        (res', _, _) = allocateRegisters st resProg
        instructions = ((mapRet sm) . (concatMap lirUnitInstructions) . lirProgUnits) res'
        blocks = makeBlocks (trace (pp resProg) instructions)

    in GMany NothingO (mapFromList (zip (map entryLabel blocks) blocks)) NothingO
  where
    mapRet sm = map (fillret sm)
    fillret sm (LIRRetInst [] meth) = LIRRetInst (smLookup sm meth) meth
    fillret sm other = other
    smLookup sm meth = case Map.lookup meth sm of
                          Just v  -> v
                          Nothing -> []

makeBlocks :: [LIRInst] -> [Block LIRNode C C]
makeBlocks insts = startBlock [] insts
  where
    startBlock  :: [Block LIRNode C C] -> [LIRInst] -> [Block LIRNode C C]
    startBlock bs [] = bs
    startBlock bs (inst:is) = 
      case inst of
        -- Closed Open nodes
        LIRLabelInst lab -> (buildBlock bs (BFirst $ LIRLabelNode $ lab) is)
        other -> (startBlock bs is) -- skip the node; it's dead code (hopefully...)

    buildBlock :: [Block LIRNode C C] -> Block LIRNode C O -> [LIRInst] -> [Block LIRNode C C]
    buildBlock bs b [] = error "unfinished block"
    buildBlock bs b (inst:is) = 
      case inst of
        -- Open Closed nodes
        LIRRetInst labels meth -> 
          startBlock ((b `BClosed` (BLast $ LIRRetNode labels meth)) : bs) is
        LIRIfInst expr falselabel truelabel -> 
          startBlock ((b `BClosed` (BLast $ LIRIfNode expr falselabel truelabel)) : bs) is
        LIRCallInst lab ret ->
          startBlock ((b `BClosed` (BLast $ LIRCallNode lab ret)) : bs) is -- add another label hopefully
        LIRJumpLabelInst lab ->
          startBlock ((b `BClosed` (BLast $ LIRJumpLabelNode lab)) : bs) is
        LIRCalloutInst name -> -- this one is borderline...
          buildBlock bs (b `BHead` (LIRCalloutNode name)) is
        LIRRegAssignInst reg expr ->
          buildBlock bs (b `BHead` (LIRRegAssignNode reg expr)) is
        LIRRegOffAssignInst reg reg' size oper ->
          buildBlock bs (b `BHead` (LIRRegOffAssignNode reg reg' size oper)) is
        LIRStoreInst mem oper ->
          buildBlock bs (b `BHead` (LIRStoreNode mem oper)) is
        LIRLoadInst reg mem ->
          buildBlock bs (b `BHead` (LIRLoadNode reg mem)) is
        LIREnterInst int ->
          buildBlock bs (b `BHead` (LIREnterNode int)) is

graphToLIR :: Graph' Block LIRNode e x -> [LIRInst]
graphToLIR (GNil) =  []
graphToLIR (GUnit unit) = blockToLIR unit
graphToLIR (GMany entry labels exit) = (mapEntry ++ mapLabel ++ mapExit)
  where
    mapEntry :: [LIRInst]
    mapEntry = case entry of
                  JustO v  -> blockToLIR v
                  NothingO -> []
    mapExit :: [LIRInst]
    mapExit = case exit of
                  JustO v  -> blockToLIR v
                  NothingO -> []
    mapLabel = concatMap (blockToLIR.snd) (mapToList labels)

blockToLIR :: Block LIRNode e x -> [LIRInst]
blockToLIR (BFirst node)   = nodeToLIR node
blockToLIR (BMiddle node)  = nodeToLIR node
blockToLIR (BLast node)    = nodeToLIR node
blockToLIR (BCat b1 b2)    = blockToLIR b1 ++ blockToLIR b2
blockToLIR (BHead b n)     = blockToLIR b  ++ nodeToLIR n
blockToLIR (BTail n b)     = nodeToLIR n   ++ blockToLIR b
blockToLIR (BClosed b1 b2) = blockToLIR b1 ++ blockToLIR b2

nodeToLIR :: LIRNode e x -> [LIRInst]
nodeToLIR (LIRLabelNode l)                = [LIRLabelInst l]
nodeToLIR (LIRRegAssignNode r e)          = [LIRRegAssignInst r e]
nodeToLIR (LIRRegOffAssignNode r1 r2 s o) = [LIRRegOffAssignInst r1 r2 s o]
nodeToLIR (LIRStoreNode m o)              = [LIRStoreInst m o]
nodeToLIR (LIRLoadNode r m)               = [LIRLoadInst r m]
nodeToLIR (LIRCalloutNode s)              = [LIRCalloutInst s]
nodeToLIR (LIREnterNode i)                = [LIREnterInst i]
nodeToLIR (LIRRetNode l m)                = [LIRRetInst l m]
nodeToLIR (LIRIfNode e fl tl)             = [LIRIfInst e fl tl]
nodeToLIR (LIRJumpLabelNode l)            = [LIRJumpLabelInst l]
nodeToLIR (LIRCallNode l1 l2)             = [LIRCallInst l1 l2]
