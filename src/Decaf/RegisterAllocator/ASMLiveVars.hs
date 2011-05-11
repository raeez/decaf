{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, NoMonomorphismRestriction #-}

module Decaf.ASMLiveVars
  ( asmLiveVars
  , VarFact
  )

where
import Decaf.IR.LIR
--import Decaf.IR.IRNode
import Decaf.HooplNodes
import Decaf.IR.ASM
import Decaf.IR.ASM
import Decaf.RegisterAllocator.GraphASM
import Loligoptl.Dataflow
import Loligoptl.Graph 
import Loligoptl.Fuel 
import Loligoptl.Label 
import Loligoptl.Combinators

import Control.Monad
import Data.Maybe
import Debug.Trace

import qualified Data.Map as M
import qualified Data.Set as S

type Key = (ASMReg, ASMNode)
type VarFact = S.Set Key 

lookupReg :: ASMReg  -> VarFact -> [Key]
lookupReg r = S.filter ((== r) . fst)

-- join two CSE facts 
join :: VarFact -> VarFact -> (ChangeFlag, VarFact)
join f g = if f == g then (NoChange, f) else (SomeChange, S.Union f g)


transfer :: BwdTransfer ASMNode VarFact
transfer = mkBTransfer livetran
  where
    livetran :: ASMNode e x -> Fact x VarFact -> VarFact

    livetran n@(ASMAddNode i op1 op2) f = opinsert op1 n (opinsert op2 n f)
    livetran n@(ASMSubNode i op1 op2) f = opinsert op1 n (opinsert op2 n f)
    livetran n@(ASMMovNode i op1 op2) f = opinsert op2 n (opdelete op1 n f) -- note mov
    livetran n@(ASMCmpNode i op1 op2) f = opinsert op1 n (opinsert op2 n f)
    livetran n@(ASMAndNode i op1 op2) f = opinsert op1 n (opinsert op2 n f)
    livetran n@(ASMOrNode  i op1 op2) f = opinsert op1 n (opinsert op2 n f)
    livetran n@(ASMXorNode i op1 op2) f = opinsert op1 n (opinsert op2 n f)
    -- what is this?
    livetran n@(ASMShlNode i op1 op2) f = opinsert op1 n (opinsert op2 n f)
    livetran n@(ASMShrNode i op1 op2) f = opinsert op1 n (opinsert op2 n f)
    livetran n@(ASMShraNode i op1 op2) f = opinsert op1 n (opinsert op2 n f)

    -- these don't actually do anything? just overwrite RA=
    livetran n@(ASMMulNode i  op) f = opinsert op n $ opinsert' RAX n $ opdelete' n RDX f
    livetran n@(ASMDivNode i  op) f = opinsert op n $ opinsert' RAX n $ opdelete' n RDX f 
    livetran n@(ASMModNode i  op) f = opinsert op n $ opinsert' RAX n $ opdelete' n RDX f 
    livetran n@(ASMPushNode i op) f = opinsert op n f
    livetran n@(ASMPopNode i  op) f = opdelete op n f
    livetran n@(ASMNegNode i  op) f = opinsert op n f
    livetran n@(ASMNotNode i  op) f = opinsert op n f

    -- following two sections probably need to switch (because it's a backwards pass)
    livetran n@(ASMJmpNode i lab) f = mapLookup lab f 
    livetran n@(ASMJeNode i  lab) f = mapLookup lab f
    livetran n@(ASMJneNode i lab) f = mapLookup lab f
    livetran n@(ASMJgNode i  lab) f = mapLookup lab f
    livetran n@(ASMJgeNode i lab) f = mapLookup lab f
    livetran n@(ASMJlNode i  lab) f = mapLookup lab f
    livetran n@(ASMJleNode i lab) f = mapLookup lab f

    -- not sure about these yet CHANGE
    livetran n@(ASMLabelNode i lab) f = f
    livetran n@(ASMCallNode  i sym) f = f
    livetran n@(ASMEnterNode i int) f = f
    livetran n@(ASMRetNode   i)     f = bottom


    -- delete doesn't actually need n
    opdelete, opinsert :: ASMOperand -> ASMNode -> VarFact -> VarFact
    opdelete (ASMRegOperand r i) n = S.filter (\(x,y) -> (x /= r))
    -- only inserts registers
    opinsert (ASMRegOperand r i) n = S.insert (r,n)
    opinsert _ _ = id

    opdelete' r n = S.filter (\(x,y) -> (x /= r))
    -- only inserts registers
    opinsert' r n = S.insert (r,n)

-- needed?
{-
    meminsert :: LIRMemAddr -> VarFact -> VarFact
    meminsert (LIRMemAddr r mr _ _) f = 
      reginsert r $
      case mr of 
        Just r' -> reginsert r' f
        Nothing -> f
-}

-- rewrite: no rewrites
rewrite :: Monad m => FwdRewrite m Node CSEFact
rewrite = shallowFwdRw (return Nothing)

-- define lattice
bottom = S.empty
lattice = DataflowLattice
  { factBottom = bottom
  , factJoin   = join }

-- define fwd pass
asmLiveVars  = BwdPass
  { bpLattice = lattice
  , bpTransfer = transfer
  , bpRewrite = rewrite }
