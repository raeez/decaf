{-# LANGUAGE PatternGuards #-}

{- NOTES

When building IGraph:

  - live vars stores all (reg,node) pairs for the current def of reg;
we assume they all lie in the same web (right?)
  - bwd analysis uses fact flowing out of node, fwd uses fact flowing in...

-}

module Decaf.RegisterAllocator.Allocator 
  ( colorRegisters
  )
where

import Decaf.RegisterAllocator.IGraph
import Loligoptl.Dataflow
import Decaf.HooplNodes
import Decaf.LiveVars


type VarDataGraph = DG VarFact Node C C

{- -> attach web information to each line of code

WEBS:
at each line, 
   add each var to the web of its reaching def(s) (a,b -> c --> join(a,c); join (b,c))
   pointer from line to web

build empty graph from webs (and precolored registers?)

TODO
-}

type Key = (ASMReg, ASMNode)

type Web = UnionSet Key -- needs to store the register and which line it's on

-- Interferance graph, cost/coalescing graph
type IGraph = LabelGraph Key Color
mkIGraph regs = mkLabelGraph regs
type CGraph = LabelGraph Key Int
mkCGraph regs = mkLabelGraph regs


-- I think type is right
makeWebs :: Graph ASMNode C C -> Web
makeWebs g = foldDataFlowFactsFwd DefFact g webadd emptyUnion
  where 
    -- add a list of operands
    webjoins :: [ASMOperand] -> DefFact -> ASMNode -> Web -> Web
    webjoins ops f n w = webjoins' (concatMap isReg ops) f n w
      where isReg (ASMRegOperand r _) = return r -- == [r]
            isReg _ = []
    webjoins' :: [ASMReg] -> DefFact -> ASMNode -> Web -> Web
    webjoins' [] f n w = w
    webjoins' (reg:regs) f n w = webjoins' regs f n $ join (reg,n) (reg,(getDef reg f)) w

    webadd :: ASMNode -> DefFact -> Web -> Web
    webadd n reachingdefs = if nmop n then webjoins ops f n else webjoins' [RDX,RAX] f n
      where 
       nmop n = 
        case n of
         n@(ASMMulNode {}) -> False
         n@(ASMDivNode {}) -> False
         n@(ASMModNode {}) -> False
         otherwise -> True
          
       ops = 
        case n of 
         n@(ASMAddNode i op1 op2) -> [op1,op2]
         n@(ASMSubNode i op1 op2) -> [op1,op2] 
         n@(ASMMovNode i op1 op2) -> [op1,op2]
         n@(ASMCmpNode i op1 op2) -> [op1,op2]
         n@(ASMAndNode i op1 op2) -> [op1,op2]
         n@(ASMOrNode  i op1 op2) -> [op1,op2]
         n@(ASMXorNode i op1 op2) -> [op1,op2]
         n@(ASMShlNode i op1 op2) -> [op1,op2]
         n@(ASMShrNode i op1 op2) -> [op1,op2]
         n@(ASMShraNode i op1 op2) -> [op1,op2]


         n@(ASMPushNode i op) -> [op]
         n@(ASMPopNode  i op) -> [op]
         n@(ASMNegNode  i op) -> [op]
         n@(ASMNotNode  i op) -> [op]
                            
         n@(ASMEnterNode i int) -> []
                          
         n@(ASMLabelNode i lab) -> []

         n@(ASMJmpNode i lab) -> []
         n@(ASMJeNode  i lab) -> []
         n@(ASMJneNode i lab) -> [] 
         n@(ASMJgNode  i lab) -> []
         n@(ASMJgeNode i lab) -> []
         n@(ASMJlNode  i lab) -> []
         n@(ASMJleNode i lab) -> [] 
    -- don't propagate 
         n@(ASMCallNode i sym -> []
         n@(ASMRetNode  i ->  []

            

makeIGraph :: Graph ASMNode C C -> Web -> (IGraph, CGraph) -- Int's represent webs
makeIGraph g us =
  let g' = snd $ foldDataFlowFactsBwd asmLiveVars g igraphjoin (mkIGraph $ listPartitions us)
  in precolor g'
  where 
    -- finds non-symbolic registers and labels them
    precolor (ig, cg) = (labelWithKey colorNonSymb ig, cg)
      where
        colorNonSymb :: Key -> Maybe ASMReg
        colorNonSymb (r,n) = 
              case r of
                ASMSREG _ -> Nothing
                physReg -> Just (Reg physReg)


    -- join function for dataflow fold
    igraphjoin :: ASMNode -> VarFact -> (IGraph, CGraph) -> (IGraph, CGraph)
    igraphjoin n livevars (igraph, cgraph) = (ijoin igraph, cjoin cgraph)
      where
        ijoin igraph = 
          if mop n then -- mul/div/mod
            foldr (uncurry addEdge) igraph 
                  (concatMap (\rx -> map (curry id $ (rx, n)) (lookupReg r livevars)) [RAX,RDX])
          else if ctrl n then 
                 igraph
               else
                 case op of
                   (ASMRegOperand r _) -> foldr (uncurry addEdge) igraph 
                                                (map (curry id $ (r,n)) (lookupReg r livevars))
                   otherwise -> igraph
        cjoin cgraph 
          | (ASMMovNode i op1 op2) <- n
          , (ASMRegOperand r1 _)   <- op1
          , (ASMRegOperand r2 _)   <- op2
          = addEdge (lift (r1,n)) (lift (r2,n)) cgraph -- get the parents in the web

          | otherwise = cgraph
        op = 
          case n of
            n@(ASMAddNode i op1 op2) -> op1
            n@(ASMSubNode i op1 op2) -> op1
            n@(ASMMovNode i op1 op2) -> op1
            n@(ASMCmpNode i op1 op2) -> op1
            n@(ASMAndNode i op1 op2) -> op1
            n@(ASMOrNode  i op1 op2) -> op1
            n@(ASMXorNode i op1 op2) -> op1
            n@(ASMShlNode i op1 op2) -> op1
            n@(ASMShrNode i op1 op2) -> op1
            n@(ASMShraNode i op1 op2) -> op1
                                         
                                         
            n@(ASMPushNode i op) -> op
            n@(ASMPopNode  i op) -> op
            n@(ASMNegNode  i op) -> op
            n@(ASMNotNode  i op) -> op
        
        mop n = 
          case n of
            n@(ASMMulNode {}) -> True
            n@(ASMDivNode {}) -> True
            n@(ASMModNode {}) -> True
            otherwise -> False

        lift key = getParent key us


-- final function
colorRegisters :: ASMProgram -> ASMProgram
colorRegisters prog@(ASMProgram pfls pxts pscs) = loop $ numberListASM prog
  where
    loop :: [(ASMInst, Int)] -> ASMProgram
    loop pr = 
      let g = graphASMList pr in
      case colorGraph regs $ makeIGraph g (makeWebs g) of
        Left spills -> loop $ spill spills pr
        Right g -> registers g pr

    regs = [RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI, R8, R9, R10, R11, R12, R13, R14, R15]

    spill :: [Key] -> [ASMNode] -> [(ASMInst, Int)]
    spill spills pr = foldl rewspill ([],n) pr
      where 
        n = foldl (\a b -> max a (snd b)) 0 pr -- largest index used
        rewspill :: ([(ASMInst,Int)],Int) -> (ASMInst, Int) -> ([(ASMInst,Int)],Int)
        rewspill (is,max) (inst, line) = 
          let slines = map getASMLine spills in
          if line `elem` slines then
            (is++[(inst,line), -- FIGURE OUT MEMORY STORES
              
inst' = 
              case inst of 
             n@(ASMAddInst op1 op2) -> redef op1 n f
             n@(ASMSubInst op1 op2) -> redef op1 n f
             n@(ASMMovInst op1 op2) -> redef op1 n f
             n@(ASMCmpInst op1 op2) -> redef op1 n f
             n@(ASMAndInst op1 op2) -> redef op1 n f
             n@(ASMOrInst  op1 op2) -> redef op1 n f
             n@(ASMXorInst op1 op2) -> redef op1 n f

             n@(ASMShlInst op1 op2)  -> redef op1 n f
             n@(ASMShrInst op1 op2)  -> redef op1 n f
             n@(ASMShraInst op1 op2) -> redef op1 n f
                                                   
             n@(ASMMulInst  op) -> redef' RDX n $ redef' RAX n f
             n@(ASMDivInst  op) -> redef' RDX n $ redef' RAX n f
             n@(ASMModInst  op) -> redef' RDX n $ redef' RAX n f
                                              
             n@(ASMPushInst op) -> f
             n@(ASMPopInst  op) -> redef op n f
             n@(ASMNegInst  op) -> redef op n f
             n@(ASMNotInst  op) -> redef op n f
            

    registers :: IGraph -> [(ASMInst, Int)] -> ASMProgram
    registers g pr = 

numberListASM :: ASMProgram -> [(ASMInst, Int)]
numberListASM prog = 
  let instructions = asmConcat $ filter isText (progSections prog)
  in zip castAsmToList [1..]
  where
    isText (ASMTextSection {}) = True
    isText _ = False



{- this moved to Dataflow.hs

varGraph :: DecafGraph C C -> VarDataGraph
varGraph g = 
  -- fst takes the DG, drops the state
  fst $ runLFM $ (afrGraph pass entries g (mapSingleton mainlab bottom)) >>= fst
  where 
    mainlab = LIRLabel "main" (-1)
    entries = JustC [mainlab]

    pass = liveVarPass
    bottom = (factBottom . fpLattice) pass
-}


{- TODO
make function that gets all facts (from every line of SSA) and returns the list

use list to generate graph

write precoloring code for special registers

coalescing
spill costs?

fix function calls

write spill code (in assembler.hs ?)
-}