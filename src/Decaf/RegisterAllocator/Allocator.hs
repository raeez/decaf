{- NOTES

When building IGraph:

  - live vars stores all (reg,node) pairs for the current def of reg;
we assume they all lie in the same web (right?)
  - bwd analysis uses fact flowing out of node, fwd uses fact flowing in...

-}

module Decaf.RegisterAllocator.Allocator 
  ( allocateRegisters
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

            


-- IGraph Int because webs are stored as integers in the unionset
-- this int is found from an asmnode by using find on the UnionSet
makeIGraph :: Graph ASMNode C C -> Web -> IGraph Key -- Int's represent webs
makeIGraph g us = snd $ foldDataFlowFactsBwd asmLiveVars g igraphjoin (mkIGraph $ listPartitions us)
  where 
    igraphjoin :: ASMNode -> VarFact
               -> Web -> IGraph Int
               -> (Web, IGraph Int)
    igraphjoin n livevars us igraph = 
      if nmop n then 
        case op of
          (ASMRegOperand r _) -> foldr (uncurry addEdge) igraph 
                                 (map (curry id $ (r,n)) (lookupReg r livevars))
          otherwise -> igraph
      else
        foldr (uncurry addEdge) igraph 
              (concatMap (\rx -> map (curry id $ (rx, n)) (lookupReg r livevars)) [RAX,RDX])
      where
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
        
    nmop n = 
      case n of
        n@(ASMMulNode {}) -> False
        n@(ASMDivNode {}) -> False
        n@(ASMModNode {}) -> False
        otherwise -> True


-- total function
colorRegisters :: ASMProgram -> ASMProgram
colorRegisters prog = 
  let g = graphASMProgram prog
      colorIG = colorIGraph numRegisters $ makeIGraph g (makeWebs g)




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