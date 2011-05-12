module Decaf.Passes.Peephole where

import Decaf.IR.ASM
import Debug.Trace

runPeepholeOptimizations :: ASMProgram -> ASMProgram
runPeepholeOptimizations prog = 
    prog{ progSections = (oldData, rewrittenText)}
  where
    rewrittenText = ASMTextSection $ castASMList $ (mkDualAnalysis branchCollapse) instructions
    oldData = fst (progSections prog)
    instructions :: [ASMInst]
    instructions = let text = case snd (progSections prog) of -- text section
                         (ASMTextSection insts) -> insts
                         otherwise -> error "text and data sections out of order"
                   in reverse $ castASMToList text

    runBranchRw = mkDualAnalysis branchCollapse
    
type PHAnalysis = [ASMInst] -> [ASMInst]
type PHDualRewrite = (ASMInst, ASMInst) -> [ASMInst]

mkDualAnalysis :: PHDualRewrite -> PHAnalysis
mkDualAnalysis rw []        = []
mkDualAnalysis rw (_:[])    = []
mkDualAnalysis rw (i:insts) =
    if run == (i:insts)  -- find fix point!
      then run
      else mkDualAnalysis rw run
  where 
    run = loop i insts
    loop _ []             = []
    loop last (x:[])      = rw (last, x)
    loop last (x:(xs)) = loop (head newInsts) (tail newInsts)
      where
        rw' = rw (last, x)
        newInsts = (rw' ++ xs)

branchCollapse :: PHDualRewrite
branchColapse (ASMJmpInst l1, ASMLabelInst l2)
    = if l1 == l2
        then trace ("collapsed " ++ show l1 ++ " and " ++ show l2) []
        else [ASMJmpInst l1, ASMLabelInst l2]
branchCollapse (a1,a2) = [a1,a2]
