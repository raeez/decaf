{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, NoMonomorphismRestriction #-}

module Decaf.ReachingDefs
  ( reachingDefsPass
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

type Key = ASMNode -- will only be 'mov type' nodes
type DefFact = S.Set Key 

-- join two CSE facts 
join :: VarFact -> VarFact -> (ChangeFlag, VarFact)
join f g = if f == g then (NoChange, f) else (SomeChange, S.Union f g)

transfer :: FwdTransfer ASMNode DefFact
transfer = mkFTransfer livetran
  where
    livetran :: ASMNode e x -> DefFact-> Fact x DefFact

    livetran n@(ASMAddNode i op1 op2) f = redef op1 n f
    livetran n@(ASMSubNode i op1 op2) f = redef op1 n f
    livetran n@(ASMMovNode i op1 op2) f = redef op1 n f
    livetran n@(ASMCmpNode i op1 op2) f = redef op1 n f
    livetran n@(ASMAndNode i op1 op2) f = redef op1 n f
    livetran n@(ASMOrNode  i op1 op2) f = redef op1 n f
    livetran n@(ASMXorNode i op1 op2) f = redef op1 n f
    -- what is this?
    livetran n@(ASMShlNode i op1 op2)  f = redef op1 n f
    livetran n@(ASMShrNode i op1 op2)  f = redef op1 n f
    livetran n@(ASMShraNode i op1 op2) f = redef op1 n f

    -- primes take a register rather than operand
    livetran n@(ASMMulNode i  op) f = redef' RDX n $ redef' RAX n f
    livetran n@(ASMDivNode i  op) f = redef' RDX n $ redef' RAX n f
    livetran n@(ASMModNode i  op) f = redef' RDX n $ redef' RAX n f

    livetran n@(ASMPushNode i op) f = f
    livetran n@(ASMPopNode  i op) f = redef op n f
    livetran n@(ASMNegNode  i op) f = redef op n f
    livetran n@(ASMNotNode  i op) f = redef op n f

    livetran n@(ASMEnterNode i int) f = f

    livetran n@(ASMLabelNode i lab) f = f

    livetran n@(ASMJmpNode i   lab) f = mkFactBase lattice [(lab, f)]
    livetran n@(ASMJeNode  i   lab) f = mkFactBase lattice [(lab, f)]
    livetran n@(ASMJneNode i   lab) f = mkFactBase lattice [(lab, f)]
    livetran n@(ASMJgNode  i   lab) f = mkFactBase lattice [(lab, f)]
    livetran n@(ASMJgeNode i   lab) f = mkFactBase lattice [(lab, f)]
    livetran n@(ASMJlNode  i   lab) f = mkFactBase lattice [(lab, f)]
    livetran n@(ASMJleNode i   lab) f = mkFactBase lattice [(lab, f)]
    -- don't propagate 
    livetran n@(ASMCallNode  i sym) f = mkFactBase lattice []
    livetran n@(ASMRetNode   i)     f = mkFactBase lattice []


    -- take out the old definition(s), insert the new one
    redef :: ASMOperand -> ASMNode -> VarFact -> VarFact
    redef (ASMRegOperand r i) n f = redef' r n f
    redef _ _ f = f
                  
    redef' :: ASMReg -> ASMNode -> VarFact -> VarFact
    redef' r n f = S.insert (r, n) $ S.filter (\(reg,_) -> reg /= r) f
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
liveVarPass  = FwdPass
  { fpLattice = lattice
  , fpTransfer = transfer
  , fpRewrite = rewrite }
