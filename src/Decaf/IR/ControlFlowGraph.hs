module Decaf.IR.ControlFlowGraph where
import Data.Int
import Decaf.IR.IRNode
import Decaf.IR.AST
import Decaf.IR.LIR
import Decaf.IR.SymbolTable
{- fix 

correct counting in symbol table
return highest count
runtime checks

-}

data CFGProgram = CFGProgram
    { cgProgLabel :: LIRLabel
    , cgProgUnits :: [CFGUnit]
    , cgProgLastLabel :: Int
    } deriving (Show, Eq)

data CFGUnit = CFGUnit
    { cgUnitLabel ::LIRLabel
    , cgUnitInstructions :: [CFGInst]
    } deriving (Show, Eq)

data CFGInst = CFGLIRInst LIRInst
            | CFGIf LIROperand JumpLabel [CFGInst] [CFGInst]
            | CFGExprInst { cgExpr :: CFGExpr }
              deriving (Show, Eq)

data CFGExpr = CFGLogExpr CFGInst LIRBinOp CFGInst LIRReg
            | CFGFlatExpr [CFGInst] LIROperand
              deriving (Show, Eq)

type JumpLabel = Int

cgOper (CFGExprInst (CFGLogExpr _ _ _ r)) = LIRRegOperand r
cgOper (CFGExprInst (CFGFlatExpr _ o)) = o
cgOper _ = error "Tried to mkBranch for improper CFGExprInst"


data ControlNode = BasicBlock [LIRInst]
                 | Branch
                   { condReg :: LIROperand
                   , branchNumber :: Int
                   , trueBlock  :: ControlPath
                   , falseBlock :: ControlPath
                   }

type ControlPath = [ControlNode]

data ControlGraph = ControlGraph {cgNodes :: [ControlPath]} -- a list of nodes for each method

mkBasicBlock :: [LIRInst] -> ControlNode
mkBasicBlock = BasicBlock

mkBranch :: LIROperand -> Int -> ControlPath -> ControlPath -> ControlNode
mkBranch op num b1 b2 = 
    Branch op num ((BasicBlock [LIRLabelInst (trueLabel num)]):b1++[BasicBlock [LIRLabelInst $ endLabel num]])
               (b2 ++ [BasicBlock [LIRJumpLabelInst $ endLabel num]]) 


-- CHANGE FROM LIRInst to new itermediate ' type
convertLIRInsts :: [CFGInst] -> ControlPath
convertLIRInsts insts = convHelp [] [] insts
  where
    convHelp :: ControlPath -> [LIRInst] -> [CFGInst] -> ControlPath
    convHelp nodes body [] = nodes ++ [mkBasicBlock body]
    convHelp nodes body (inst:is) =
        case inst of
          CFGLIRInst inst@(LIRCallInst{}) -> convHelp (nodes ++ (pushBlock' inst)) [] is -- fix this?
          CFGLIRInst inst@LIRRetInst -> convHelp (nodes ++ (pushBlock' inst)) [] is
          CFGIf reg label block eblock ->
              convHelp (nodes ++ pushBlock ++ [mkBranch reg label (convHelp [] [] block) (convHelp [] [] eblock)]) [] is
          CFGExprInst (CFGLogExpr expr1 op expr2 reg) -> -- reg is used for labeling new branches
            let contEvaling   = (convHelp [] [] ([expr2] ++ [CFGLIRInst $ LIRRegAssignInst reg (LIROperExpr $ (cgOper expr2))]))
                retValBlock b = [mkBasicBlock [LIRRegAssignInst reg (LIROperExpr (LIRIntOperand b))]]
            in
              case op of
-- add code to fill in final value in reg
                LAND -> convHelp (nodes ++ pushBlock ++ (convHelp [] [] [expr1])
                                  ++[mkBranch (cgOper expr1) 
                                              (symToInt reg)
                                              contEvaling
                                              (retValBlock asmFalse)]) [] is
                LOR -> convHelp (nodes ++ pushBlock ++ (convHelp [] [] [expr1])
                                  ++[mkBranch (cgOper expr1) 
                                              (symToInt reg)
                                              (retValBlock asmTrue)
                                              contEvaling]) [] is

          CFGExprInst (CFGFlatExpr insts _) ->
              convHelp nodes body (insts ++is)

          CFGLIRInst x -> convHelp nodes (body++[x]) is


         where 
           pushBlock' inst = [mkBasicBlock (body++[inst])]
           pushBlock       =  [mkBasicBlock body]


pullOutLIR (CFGLIRInst x) = x

symToInt :: LIRReg -> Int
symToInt (SREG s) = s
symToInt  reg = error "attempted to label if using non-symbolic register"
--symToInt (LIRIntOperand x) = error "attempted to label if using int literal"

convertProgram :: CFGProgram -> ControlGraph
convertProgram prog =
    ControlGraph (map (convertLIRInsts.g) (cgProgUnits prog))
  where g (CFGUnit lab insts) = (CFGLIRInst $ LIRLabelInst lab) : insts

translateCFG :: ControlGraph -> LIRProgram
translateCFG g = LIRProgram (LIRLabel "prog" 0) $ map ((LIRUnit (LIRLabel "" 0)).concatMap h) (cgNodes g)
  where 
    h :: ControlNode -> [LIRInst]
    h (BasicBlock insts) = insts
    h (Branch {condReg = reg, trueBlock = tb, falseBlock = fb, branchNumber = num}) = 
        [(LIRIfInst (LIROperRelExpr reg) (trueLabel num))] ++ (concatMap h fb) ++ (concatMap h tb)


{-CFTtoLOLG :: ControlGraph -> LolGraph
CFTtoLOLG (ControlGraph paths) = foldl dgSplice GNil (map (h  paths)
  where-}
