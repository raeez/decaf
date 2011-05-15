{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, NoMonomorphismRestriction#-}
module Decaf.RegisterAllocator.TrivialPass (trivialPass) where
import Loligoptl.Dataflow
import Loligoptl.Graph 
import Loligoptl.Fuel 
import Loligoptl.Label 
import Loligoptl.Combinators

import Decaf.IR.ASM
import Decaf.RegisterAllocator.GraphASM

import Debug.Trace

import qualified Data.Set as S

type TF = S.Set (ASMReg, ASMNode)

join lab (OldFact f) (NewFact g) = (NoChange, f `S.union` g)


transfer :: FwdTransfer ASMNode' TF
transfer = mkFTransfer livetran''
  where
    livetran'' n f = trace (show $ getASMLine $ asmDropPrime n) $ livetran n f
    livetran :: ASMNode' e x -> TF -> Fact x TF

    livetran n@(ASMAddNode' i op1 op2) f = f
    livetran n@(ASMSubNode' i op1 op2) f = f
    livetran n@(ASMMovNode' i op1 op2) f = f
    livetran n@(ASMCmpNode' i op1 op2) f = f
    livetran n@(ASMAndNode' i op1 op2) f = f
    livetran n@(ASMOrNode'  i op1 op2) f = f
    livetran n@(ASMXorNode' i op1 op2) f = f

    livetran n@(ASMShlNode' i op1 op2)  f = f
    livetran n@(ASMShrNode' i op1 op2)  f = f
    livetran n@(ASMShraNode' i op1 op2) f = f

    -- primes take a register rather than operand
    livetran n@(ASMMulNode' i  op) f = f
    livetran n@(ASMDivNode' i  op) f = f
    livetran n@(ASMModNode' i  op) f = f

    livetran n@(ASMPushNode' i op) f = f
    livetran n@(ASMPopNode'  i op) f = f
    livetran n@(ASMNegNode'  i op) f = f
    livetran n@(ASMNotNode'  i op) f = f

    livetran n@(ASMEnterNode' i int) f = f

    livetran n@(ASMLabelNode' i lab) f = f

    livetran n@(ASMJmpNode' i lab) f   = mkFactBase lattice [(asmLabeltoHoopl lab, f)] 
    livetran n@(ASMJeNode'  i lab o) f = mkFactBase lattice [(asmLabeltoHoopl lab, f),
                                                             (asmLabeltoHoopl o, f)]
    livetran n@(ASMJneNode' i lab o) f = mkFactBase lattice [(asmLabeltoHoopl lab, f), (asmLabeltoHoopl o, f)]
    livetran n@(ASMJgNode'  i lab o) f = mkFactBase lattice [(asmLabeltoHoopl lab, f), (asmLabeltoHoopl o, f)]
    livetran n@(ASMJgeNode' i lab o) f = mkFactBase lattice [(asmLabeltoHoopl lab, f), (asmLabeltoHoopl o, f)]
    livetran n@(ASMJlNode'  i lab o) f = mkFactBase lattice [(asmLabeltoHoopl lab, f), (asmLabeltoHoopl o, f)]
    livetran n@(ASMJleNode' i lab o) f = mkFactBase lattice [(asmLabeltoHoopl lab, f), (asmLabeltoHoopl o, f)]

    livetran n@(ASMCallNode'  i sym lab) f = mkFactBase lattice [(asmLabeltoHoopl lab, f)]
    livetran n@(ASMRetNode'   i)     f = mkFactBase lattice []



-- rewrite: no rewrites
rewrite :: FuelMonad m => FwdRewrite m ASMNode' TF
rewrite = deepFwdRw (\n f -> return Nothing)

-- define lattice
bottom = S.empty
lattice = DataflowLattice
  { fact_name = "Trivial Pass"
  , fact_bot = bottom
  , fact_join = join }

-- define fwd pass
trivialPass  = FwdPass
  { fp_lattice = lattice
  , fp_transfer = transfer
  , fp_rewrite = rewrite }
