{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, NoMonomorphismRestriction #-}

module Decaf.LiveVars 
  ( liveVarPass
--  , VarFact
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

type Key = ASMReg
type VarFact = S.Set Key 

-- join two CSE facts 
join :: VarFact -> VarFact -> (ChangeFlag, VarFact)
join f g = if f == g then (NoChange, f) else (SomeChange, S.Union f g)


transfer :: BwdTransfer ASMNode VarFact
transfer = mkBTransfer livetran
  where
    livetran :: ASMNode e x -> VarFact -> Fact x VarFact

    livetran (ASMAddNode i op1 op2) f = opinsert op1 (opinsert op2 f)
    livetran (ASMSubNode i op1 op2) f = opinsert op1 (opinsert op2 f)
    livetran (ASMMovNode i op1 op2) f = opinsert op2 (opdelete op1 f)
    livetran (ASMCmpNode i op1 op2) f = opinsert op1 (opinsert op2 f)
    livetran (ASMAndNode i op1 op2) f = opinsert op1 (opinsert op2 f)
    livetran (ASMOrNode  i op1 op2) f = opinsert op1 (opinsert op2 f)
    livetran (ASMXorNode i op1 op2) f = opinsert op1 (opinsert op2 f)
    -- what is this?
    livetran (ASMShlNode i op1 op2) f = opinsert op1 (opinsert op2 f)
    livetran (ASMShrNode i op1 op2) f = opinsert op1 (opinsert op2 f)
    livetran (ASMShraNode i op1 op2) f = opinsert op1 (opinsert op2 f)

    -- these don't actually do anything? just overwrite RA=
    livetran (ASMMulNode i  op) f = opinsert op $ opinsert RAX $ opdelete RDX f
    livetran (ASMDivNode i  op) f = opinsert op $ opinsert RAX $ opdelete RDX f 
    livetran (ASMModNode i  op) f = opinsert op $ opinsert RAX $ opdelete RDX f 
    livetran (ASMPushNode i op) f = opinsert op f
    livetran (ASMPopNode i  op) f = opdelete op f
    livetran (ASMNegNode i  op) f = opinsert op f
    livetran (ASMNotNode i  op) f = opinsert op f



    -- following two sections probably need to switch (because it's a backwards pass)
    livetran (ASMJmpNode i   lab) f = mkFactBase lattice [(lab, f)]
    livetran (ASMJeNode i    lab) f = mkFactBase lattice [(lab, f)]
    livetran (ASMJneNode i   lab) f = mkFactBase lattice [(lab, f)]
    livetran (ASMJgNode i    lab) f = mkFactBase lattice [(lab, f)]
    livetran (ASMJgeNode i   lab) f = mkFactBase lattice [(lab, f)]
    livetran (ASMJlNode i    lab) f = mkFactBase lattice [(lab, f)]
    livetran (ASMJleNode i   lab) f = mkFactBase lattice [(lab, f)]

    -- not sure about these yet CHANGE
    livetran (ASMLabelNode i lab) f = f
    livetran (ASMCallNode  i sym) f = f
    livetran (ASMEnterNode i int) f = f
    livetran (ASMRetNode   i)     f = f



    opdelete (ASMRegOperand r i) = S.delete r
    -- only inserts registers
    opinsert :: ASMOperand -> VarFact -> VarFact
    opinsert (ASMRegOperand r i) = reginsert r
    opinsert _ = id
    reginsert = S.insert

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
liveVarPass  = BwdPass
  { bpLattice = lattice
  , bpTransfer = transfer
  , bpRewrite = rewrite }
