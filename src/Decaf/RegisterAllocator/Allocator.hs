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
scan hoopl graph, updating reaching def fact
at each line, 
   add each var to the web of its reaching def(s) (a,b -> c --> join(a,c); join (b,c))
   pointer from line to web

build empty graph from webs (and precolored registers?)


TODO
number each line of graph
build webs
-}

-- I think type is right
makeWebs :: Graph ASMNode C C -> UnionSet (ASMNode, Int)
makeWebs g = foldDataFlowFacts ASMReachingpass g webjoin emptyUnion
  where webjoin (n,i) reachingdefs us = 
          case n of 
            -- asm node types...


-- IGraph Int because webs are stored as integers in the unionset
-- this int is found from an asmnode by using find on the UnionSet
makeIGraph :: Graph ASMNode C C -> UnionSet (ASMNode, Int) -> IGraph Int
makeIGraph g us = snd $ foldDataFlowFacts ASMLiveVars g igraphjoin (mkIGraph $ listWebs us)
  where 
    igraphjoin :: (ASMNode, Int) -> ASMLiveVarFact 
               -> (UnionSet (ASMNode, Int), IGraph Int)
               -> (UnionSet (ASMNode, Int), IGraph Int)
    igraphjoin (n,i) livevars (us, igraph) = 
      case n of
        -- asm node types...
        



colorRegisters :: ASMProgram -> ASMProgram
colorRegisters prog = 
  let asmg = graphASM
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