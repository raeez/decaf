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
                   { condReg :: LIRReg
                   , trueBlock  :: ControlPath
                   , falseBlock :: ControlPath
                   , branchNum :: Int
                   }

type ControlPath = [ControlNode]

mkBasicBlock :: [LIRInst] -> ControlNode
mkBasicBlock = BasicBlock

mkBranch :: ControlPath -> ControlPath -> ControlNode
mkBranch = Branch

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
    convHelp :: ControlPath -> [LIRInst] -> [LIRInst] -> ControlPath
    convHelp nodes body [] = nodes ++ [mkBasicBlock body]
    convHelp nodes body (inst:is) =
        case inst of
          CGLIRInst (LIRCallInst {}) -> convHelp (nodes ++ [mkBasicBlock (body++inst)]) [] is -- fix this
          CGLIRInst LIRRetInst -> convHelp (nodes++ [mkBasicBlock (body++inst)]) [] is
          CGIf reg block melse num ->
              convHelp (nodes ++ [mkBranch reg (convHelp [] [] block) eb num]) [] is
                where eb = case melse of 
                             Just block -> convHelp [] [] block
                             Nothing -> []
          CGExprInst (CGLogExpr expr1 op expr2) reg ->
              case op of
-- add code to fill in final value in reg
                LAND -> convHelp (nodes++(convHelp [] [] [expr1])
                                  ++[mkBranch (exprReg expr1) 
                                              contEvaling
                                              retValBlock 0
                                              reg]) [] is
                LOR -> convHelp (nodes++(convHelp [] [] [expr1])
                                  ++[mkBranch (exprReg expr1) 
                                              retValBlock 1
                                              contEvaling 
                                              reg]) [] is

            where contEvaling   = (convHelp [] [] [expr2]) ++ 
                    (LIRRegAssignInst reg (LIROperExpr.LIRRegOperand $ (exprReg expr2)))
                  retValBlock b = [mkBasicBlock [LIRRegAssignInst reg (LIROperExpr (LIRIntOperand (LIRInt b)))]]

          CGExprInst (CGFlatExpr insts) _ ->
              convHelp nodes (body ++ insts)

          otherwise -> convHelp nodes (body++[inst]) is



convertProgram :: LIRProgram -> ControlGraph
convertProgram prog =
    ControlGraph (map (convertLIRInsts.g) (progUnits prog))
  where g (LIRUnit lab insts) = (LIRLabelInst lab) : insts


translateCG :: ControlGraph -> [LIRInst]
translateCG g = concatMap (map h) (cgNodes g)
  where 
    h (BasicBlock insts) = insts
    h (Branch {condReg = reg, trueBlock = tb, falseBlock = fb, branchNum = num}) = 
        (LIRIfInst reg (trueLabel num)) ++ (h fb) ++ (h tb)



