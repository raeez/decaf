{-# LANGUAGE MultiParamTypeClasses #-}

module Decaf.IR.ControlFlowGraph where
--import Decaf.IR.Class
import Data.Int
import Decaf.IR.IRNode
import Decaf.IR.AST
import Decaf.IR.LIR
import Decaf.IR.SymbolTable
import Decaf.Translator
import Decaf.IR.DecafGraph
import Decaf.HooplNodes

import Loligoptl.Graph
import Loligoptl.Label 

import Control.Monad
import Data.Int
{- fix 

correct counting in symbol table
return highest count
runtime checks

-}

mkBasicBlock :: [LIRInst] -> ControlNode
mkBasicBlock = BasicBlock

{-mkBranch :: LIROperand -> Int -> ControlPath -> ControlPath -> ControlNode
mkBranch op num b1 b2 = 
    Branch op num ((BasicBlock [LIRLabelInst (trueLabel num)]):b1++[BasicBlock [LIRLabelInst $ endLabel num]])
               (b2 ++ [BasicBlock [LIRJumpLabelInst $ endLabel num]]) 
-}

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
{-convertLIRInsts :: [CFGInst] -> Translator ControlPath
convertLIRInsts insts = convHelp [] [] insts
  where
    convHelp :: ControlPath -> [LIRInst] -> [CFGInst] -> Translator ControlPath
    convHelp nodes body [] = return $ nodes ++ [mkBasicBlock body]
    convHelp nodes body (inst:is) = 
        case inst of
          CFGLIRInst inst@(LIRCallInst{}) -> convHelp (nodes ++ (pushBlock' inst)) [] is -- fix this?
          CFGLIRInst inst@LIRRetInst -> convHelp (nodes ++ (pushBlock' inst)) [] is
{-          CFGIf reg label block eblock ->
              convHelp (nodes ++ pushBlock ++ [mkBranch reg label (convHelp [] [] block) (convHelp [] [] eblock)]) [] is-}
          CFGExprInst (CFGLogExpr expr1 op expr2 reg) -> -- reg is used for labeling new branches
            do contEvaling  <- convHelp [] [] ([expr2] ++ [CFGLIRInst $ LIRRegAssignInst reg (LIROperExpr $ (cgOper expr2))])
               let retValBlock b = [mkBasicBlock [LIRRegAssignInst reg (LIROperExpr (LIRIntOperand b))]]
               case op of
-- add code to fill in final value in reg
                LAND -> do 
                  lab <- incLabel
                  expr1' <- (convHelp [] [] [expr1])
                  convHelp (nodes ++ pushBlock ++ expr1'
                                  ++[mkBranch (cgOper expr1) 
                                              lab
--                                              (symToInt reg)
                                              contEvaling
                                              (retValBlock asmFalse)]) [] is
                LOR -> do
                  lab <- incLabel
                  expr1' <- (convHelp [] [] [expr1])
                  convHelp (nodes ++ pushBlock ++ expr1'
                                  ++[mkBranch (cgOper expr1) 
                                              lab 
--                                              (symToInt reg)
                                              (retValBlock asmTrue)
                                              contEvaling]) [] is

          CFGExprInst (CFGFlatExpr insts _) ->
              convHelp nodes body (insts ++is)

          CFGLIRInst x -> convHelp nodes (body++[x]) is


         where 
           pushBlock' inst = [mkBasicBlock (body++[inst])]
           pushBlock       =  [mkBasicBlock body]

-}
pullOutLIR (CFGLIRInst x) = x

symToInt :: LIRReg -> Int
symToInt (SREG s) = s
symToInt  reg = error "attempted to label if using non-symbolic register"
--symToInt (LIRIntOperand x) = error "attempted to label if using int literal"


-- | Main function.  Takes HIR and turns it into Hoopl graph
graphProgram :: SymbolTree -> DecafProgram -> DecafGraph C C
graphProgram st dp =
  let g (CFGUnit lab insts) = (CFGLIRInst $ LIRLabelInst lab) : insts 
      (res, _) = runTranslator (do prog <- translateProgram st dp
                                   mapM (shortCircuit.g) (cgProgUnits prog) 
                                          >>= (return . stripCFG . concat))
                               (mkNamespace 0)
      blocks = makeBlocks res
        where
          -- inserts labels for function jumps

  in GMany NothingO (mapFromList $ zip (map entryLabel blocks) blocks)  NothingO -- should change probably

{-cfgGraphProgram :: CFGProgram -> Translator ControlGraph
cfgGraphProgram prog = 
  mapM (convertLIRInsts.g) (cgProgUnits prog) >>= (return.ControlGraph)
  where g (CFGUnit lab insts) = (CFGLIRInst $ LIRLabelInst lab) : insts -- inserts labels for function jumps
-}
{-translateCFG :: ControlGraph -> LIRProgram
translateCFG g = LIRProgram (LIRLabel "prog" 0) $ map ((LIRUnit (LIRLabel "" 0)).concatMap h) (cgNodes g)
  where 
    h :: ControlNode -> [LIRInst]
    h (BasicBlock insts) = insts
    h (Branch {condReg = reg, trueBlock = tb, falseBlock = fb, branchNumber = num}) = 
        [(LIRIfInst (LIROperRelExpr reg) (falseLabel num) (trueLabel num))] ++ (concatMap h fb) ++ (concatMap h tb)
-}

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
          startBlock ((b `BClosed` (BLast $ LIRCallNode [lab])) : bs) is -- add another label hopefully
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


        
        
{-CFTtoLOLG :: ControlGraph -> LolGraph
CFTtoLOLG (ControlGraph paths) = foldl dgSplice GNil (map (h  paths)
  where-}
