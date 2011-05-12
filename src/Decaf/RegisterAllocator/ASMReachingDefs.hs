{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, NoMonomorphismRestriction #-}

module Decaf.RegisterAllocator.ASMReachingDefs
  ( asmReachingDefs
  , DefFact
  , getDef
  )

where
import Decaf.IR.LIR
--import Decaf.IR.IRNode
import Decaf.LIRNodes
import Decaf.IR.ASM
import Decaf.IR.ASM
import Decaf.RegisterAllocator.GraphASM
import Loligoptl.Dataflow
import Loligoptl.Graph 
import Loligoptl.Fuel 
import Loligoptl.Label 
import Loligoptl.Combinators

import Control.Monad hiding (join)
import Data.Maybe
import Debug.Trace

import qualified Data.Map as M
import qualified Data.Set as S

type DefFact = S.Set (ASMReg, ASMNode)

getDef :: ASMReg -> DefFact -> Maybe ASMNode
getDef r f =  lookup r $ S.toAscList f -- returns the first one
{-               Nothing -> error ("tried to look up register without preceeding definition in ASMReachingDefs line 32 " ++ (show r))
               Just n -> n
-}
join lab (OldFact f) (NewFact g) = if f == g then (NoChange, f) else (SomeChange, S.union f g)

transfer :: FwdTransfer ASMNode' DefFact
transfer = mkFTransfer livetran
  where
    livetran :: ASMNode' e x -> DefFact-> Fact x DefFact

    livetran n@(ASMAddNode' i op1 op2) f = redef op1 n f
    livetran n@(ASMSubNode' i op1 op2) f = redef op1 n f
    livetran n@(ASMMovNode' i op1 op2) f = redef op1 n f
    livetran n@(ASMCmpNode' i op1 op2) f = redef op1 n f
    livetran n@(ASMAndNode' i op1 op2) f = redef op1 n f
    livetran n@(ASMOrNode'  i op1 op2) f = redef op1 n f
    livetran n@(ASMXorNode' i op1 op2) f = redef op1 n f

    livetran n@(ASMShlNode' i op1 op2)  f = redef op1 n f
    livetran n@(ASMShrNode' i op1 op2)  f = redef op1 n f
    livetran n@(ASMShraNode' i op1 op2) f = redef op1 n f

    -- primes take a register rather than operand
    livetran n@(ASMMulNode' i  op) f = redef' RDX n $ redef' RAX n f
    livetran n@(ASMDivNode' i  op) f = redef' RDX n $ redef' RAX n f
    livetran n@(ASMModNode' i  op) f = redef' RDX n $ redef' RAX n f

    livetran n@(ASMPushNode' i op) f = f
    livetran n@(ASMPopNode'  i op) f = redef op n f
    livetran n@(ASMNegNode'  i op) f = redef op n f
    livetran n@(ASMNotNode'  i op) f = redef op n f

    livetran n@(ASMEnterNode' i int) f = f

    livetran n@(ASMLabelNode' i lab) f = f

    livetran n@(ASMJmpNode' i lab o) f = mkFactBase lattice [(asmLabeltoHoopl lab, f)
                                                            ,(asmLabeltoHoopl o, f)]
    livetran n@(ASMJeNode'  i lab o) f = mkFactBase lattice [(asmLabeltoHoopl lab, f)
                                                            ,(asmLabeltoHoopl o, f)]
    livetran n@(ASMJneNode' i lab o) f = mkFactBase lattice [(asmLabeltoHoopl lab, f)
                                                            ,(asmLabeltoHoopl o, f)]
    livetran n@(ASMJgNode'  i lab o) f = mkFactBase lattice [(asmLabeltoHoopl lab, f)
                                                            ,(asmLabeltoHoopl o, f)]
    livetran n@(ASMJgeNode' i lab o) f = mkFactBase lattice [(asmLabeltoHoopl lab, f)
                                                            ,(asmLabeltoHoopl o, f)]
    livetran n@(ASMJlNode'  i lab o) f = mkFactBase lattice [(asmLabeltoHoopl lab, f)
                                                            ,(asmLabeltoHoopl o, f)]
    livetran n@(ASMJleNode' i lab o) f = mkFactBase lattice [(asmLabeltoHoopl lab, f)
                                                            ,(asmLabeltoHoopl o, f)]
    -- don't propagate 
    livetran n@(ASMCallNode'  i sym) f = mkFactBase lattice []
    livetran n@(ASMRetNode'   i)     f = mkFactBase lattice []


    -- take out the old definition(s), insert the new one
    redef :: ASMOperand -> ASMNode' e x -> DefFact -> DefFact
    redef (ASMRegOperand r i) n f = redef' r n f
    redef _ _ f = f
                  
    redef' :: ASMReg -> ASMNode' e x -> DefFact -> DefFact
    redef' r n f = S.insert (r, asmDropPrime n) $ S.filter (\(reg,_) -> reg /= r) f
-- needed?
{-
    meminsert :: LIRMemAddr -> DefFact -> DefFact
    meminsert (LIRMemAddr r mr _ _) f = 
      reginsert r $
      case mr of 
        Just r' -> reginsert r' f
        Nothing -> f
-}

-- rewrite: no rewrites
rewrite :: FuelMonad m => FwdRewrite m ASMNode' DefFact
rewrite = deepFwdRw (\n f -> return Nothing)

-- define lattice
bottom = S.empty
lattice = DataflowLattice
  { fact_name = "ASM Reaching Definitions"
  , fact_bot = bottom
  , fact_join = join }

-- define fwd pass
asmReachingDefs  = FwdPass
  { fp_lattice = lattice
  , fp_transfer = transfer
  , fp_rewrite = rewrite }
