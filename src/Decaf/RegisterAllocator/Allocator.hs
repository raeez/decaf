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


makeWebs :: DG ReachFact ASMNode C C
         -> 







-- change decafgraph to SSA graph?
varGraph :: DecafGraph C C -> VarDataGraph
varGraph g = 
  -- fst takes the DG, drops the state
  fst $ runLFM $ (afrGraph pass entries g (mapSingleton mainlab bottom)) >>= fst
  where 
    mainlab = LIRLabel "main" (-1)
    entries = JustC [mainlab]

    pass = liveVarPass
    bottom = (factBottom . fpLattice) pass



{- TODO
make function that gets all facts (from every line of SSA) and returns the list

use list to generate graph

write precoloring code for special registers

fix function calls

write spill code (in assembler.hs ?)
-}