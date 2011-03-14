module Decaf.IR.ControlGraph where
import Decaf.Data.SymbolTable
import Decaf.IR.Class
import Decaf.IR.AST
import Decaf.IR.LIR
import Decaf.Data.Tree
import Decaf.Data.Zipper
import Decaf.Translator



data ControlNode
    = ControlNode
      { cnLabel :: LIRLabel
      , cnStmts :: [LIRInst]
      , cnEnd :: (ControlNodeEnd, [ControlNode])
      }

data ControlNodeEnd
    = Next 
    | Branch
    | Call LIRLabel
    | Return
    | Jump LIRLabel

--data ControlGraph = (ControlNode, CGContext)


{-trueBranch :: ControlGraph -> ControlGraph
trueBranch g | cnEnd (getContent g) == Branch
    = select 0 g
falseBranch g | cnEnd (getContent g) == Branch
    = select 1 g
-}


-- continuation style
convertLIRInsts :: [LIRInst] -> ControlNode -> ControlNode
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
            LIRIfInst expr _ block elseblock ->
                (body, (Next, (ControlNode "" [] (Branch, [convertLIRInsts block next', convertLIRInsts elseblock next']))))
            LIRLabelInst label -> (body, (Next, (uncurry (ControlNode label)) $ convHelp [] is)) -- kind of weird


convertLIRExpr (LIRBinExpr op1 op op2) = 
    case op of
      LAND -> Node
                        


