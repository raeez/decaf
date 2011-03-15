module Decaf.IR.ControlGraph where
import Decaf.Data.SymbolTable
import Decaf.IR.Class
import Decaf.IR.AST
import Decaf.IR.LIR
import Decaf.Translator
{- fix 

correct counting in symbol table
return highest count
runtime checks

-}


data ControlNode = BasicBlock [LIRInst]
                 | Branch
                   { condReg :: LIROperand
                   , branchNumber :: Int
                   , trueBlock  :: ControlPath
                   , falseBlock :: ControlPath
                   }

type ControlPath = [ControlNode]

mkBasicBlock :: [LIRInst] -> ControlNode
mkBasicBlock = BasicBlock

mkBranch :: LIROperand -> Int -> ControlPath -> ControlPath -> ControlNode
mkBranch op num b1 b2 = 
    Branch op num ((BasicBlock [LIRLabelInst (trueLabel num)]):b1++[BasicBlock [LIRLabelInst $ endLabel num]])
               (b2 ++ [BasicBlock [LIRJumpLabelInst $ endLabel num]]) 


data ControlGraph = ControlGraph {cgNodes :: [ControlPath]} -- a list of nodes for each method


{-
-- continuation style
convertLIRInsts :: [LIRInst] -> ControlNode -> [ControlNode]
convertLIRInsts insts next = (uncurry (ControlNode "")) $ convHelp [] insts
  where
    convHelp body [] = (
    convHelp body (inst:is) = 
        let next' = convertLIRInsts is next 
        in
          case inst of
            LIRCallInst {- label -} -> (body, (Call, [next']))
            LIRRetInst -> (body, (Return, [next']))
            {- other jumps, giving label -}
            -- assumes that block already has jump to end label, that
            -- else block hsa end label following it
            LIRIfInst expr _ block (Maybe elseblock) ->
                (body, (Next, (Branch (exprReg expr) (label block) (label elseblock),  [convertLIRInsts block next', convertLIRInsts elseblock next']))))
            LIRLabelInst label -> (body, (Next, (uncurry (ControlNode label)) $ convHelp [] is)) -- kind of weird
-}

-- CHANGE FROM LIRInst to new itermediate ' type
convertLIRInsts :: [CGInst] -> ControlPath
convertLIRInsts insts = convHelp [] [] insts
  where
    convHelp :: ControlPath -> [LIRInst] -> [CGInst] -> ControlPath
    convHelp nodes body [] = nodes ++ [mkBasicBlock body]
    convHelp nodes body (inst:is) =
        case inst of
          CGLIRInst inst@(LIRCallInst{}) -> convHelp (nodes ++ (pushBlock' inst)) [] is -- fix this?
          CGLIRInst inst@(LIRCallAssignInst{}) -> convHelp (nodes ++ (pushBlock' inst)) [] is -- fix this?
          CGLIRInst inst@LIRRetInst -> convHelp (nodes ++ (pushBlock' inst)) [] is
          CGIf reg label block eblock ->
              convHelp (nodes ++ pushBlock ++ [mkBranch reg label (convHelp [] [] block) (convHelp [] [] eblock)]) [] is
          CGExprInst (CGLogExpr expr1 op expr2 reg) -> -- reg is used for labeling new branches
            let contEvaling   = (convHelp [] [] ([expr2] ++ [CGLIRInst $ LIRRegAssignInst reg (LIROperExpr $ (cgOper expr2))]))
                retValBlock b = [mkBasicBlock [LIRRegAssignInst reg (LIROperExpr (LIRIntOperand (LIRInt b)))]]
            in
              case op of
-- add code to fill in final value in reg
                LAND -> convHelp (nodes ++ pushBlock ++ (convHelp [] [] [expr1])
                                  ++[mkBranch (cgOper expr1) 
                                              (symToInt reg)
                                              contEvaling
                                              (retValBlock 0)]) [] is
                LOR -> convHelp (nodes ++ pushBlock ++ (convHelp [] [] [expr1])
                                  ++[mkBranch (cgOper expr1) 
                                              (symToInt reg)
                                              (retValBlock 1)
                                              contEvaling]) [] is

          CGExprInst (CGFlatExpr insts _) ->
              convHelp nodes body (insts ++is)

          CGLIRInst x -> convHelp nodes (body++[x]) is


         where 
           pushBlock' inst = [mkBasicBlock (body++[inst])]
           pushBlock       =  [mkBasicBlock body]


pullOutLIR (CGLIRInst x) = x

symToInt :: LIRReg -> Int
symToInt (SREG s) = read s :: Int
symToInt  reg = error "attempted to label if using non-symbolic register"
--symToInt (LIRIntOperand x) = error "attempted to label if using int literal"

convertProgram :: CGProgram -> ControlGraph
convertProgram prog =
    ControlGraph (map (convertLIRInsts.g) (cgProgUnits prog))
  where g (CGUnit lab insts) = (CGLIRInst $ LIRLabelInst lab) : insts


translateCG :: ControlGraph -> LIRProgram
translateCG g = LIRProgram (LIRLabel "prog") $ map ((LIRUnit (LIRLabel "")).concatMap h) (cgNodes g)
  where 
    h :: ControlNode -> [LIRInst]
    h (BasicBlock insts) = insts
    h (Branch {condReg = reg, trueBlock = tb, falseBlock = fb, branchNumber = num}) = 
        [(LIRIfInst (LIROperRelExpr reg) (trueLabel num))] ++ (concatMap h fb) ++ (concatMap h tb)



