{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, NoMonomorphismRestriction #-}
module Decaf.RegisterAllocator.ASMLiveVars
  ( asmLiveVars
  , VarFact
  , lookupReg
  )
where
import Decaf.IR.LIR
--import Decaf.IR.IRNode
import Decaf.LIRNodes
import Decaf.IR.ASM
import Decaf.IR.ASM
import Decaf.RegisterAllocator.GraphASM
import Loligoptl

import Control.Monad hiding (join)
import Data.Maybe
import Debug.Trace

import qualified Data.Map as M
import qualified Data.Set as S

type Key = (ASMReg, ASMNode)
type VarFact = S.Set Key 

lookupReg :: ASMReg  -> VarFact -> [Key]
lookupReg r f = S.toList $ S.filter ((== r) . fst) f

-- join two CSE facts 
join lab (OldFact f) (NewFact g) = if f == g then (NoChange, f) else (SomeChange, S.union f g)


transfer :: BwdTransfer ASMNode' VarFact
transfer = mkBTransfer livetran
  where
    livetran :: ASMNode' e x -> Fact x VarFact -> VarFact
    livetran n@(ASMLabelNode' i lab) f = f

    livetran n@(ASMAddNode' i op1 op2) f = opinsert op1 n (opinsert op2 n f)
    livetran n@(ASMSubNode' i op1 op2) f = opinsert op1 n (opinsert op2 n f)
    livetran n@(ASMMovNode' i op1 op2) f = opinsert op2 n (opdelete op1 n f) -- note mov
    livetran n@(ASMCmpNode' i op1 op2) f = opinsert op1 n (opinsert op2 n f)
    livetran n@(ASMAndNode' i op1 op2) f = opinsert op1 n (opinsert op2 n f)
    livetran n@(ASMOrNode'  i op1 op2) f = opinsert op1 n (opinsert op2 n f)
    livetran n@(ASMXorNode' i op1 op2) f = opinsert op1 n (opinsert op2 n f)
    -- what is this?
    livetran n@(ASMShlNode' i op1 op2) f = opinsert op1 n (opinsert op2 n f)
    livetran n@(ASMShrNode' i op1 op2) f = opinsert op1 n (opinsert op2 n f)
    livetran n@(ASMShraNode' i op1 op2) f = opinsert op1 n (opinsert op2 n f)

    -- these don't actually do anything? just overwrite RA=
    livetran n@(ASMMulNode' i  op) f = opinsert op n $ opinsert' RAX n $ opdelete' RDX n f
    livetran n@(ASMDivNode' i  op) f = opinsert op n $ opinsert' RAX n $ opdelete' RDX n f 
    livetran n@(ASMModNode' i  op) f = opinsert op n $ opinsert' RAX n $ opdelete' RDX n f 
    livetran n@(ASMPushNode' i op) f = opinsert op n f
    livetran n@(ASMPopNode' i  op) f = opdelete op n f
    livetran n@(ASMNegNode' i  op) f = opinsert op n f
    livetran n@(ASMNotNode' i  op) f = opinsert op n f

    livetran n@(ASMEnterNode' i int) f = f

    -- following two sections probably need to switch (because it's a backwards pass)
    livetran n@(ASMJmpNode' i lab) f = fromJust $ mapLookup (asmLabeltoHoopl lab) f 
    livetran n@(ASMJeNode' i  lab) f = fromJust $ mapLookup (asmLabeltoHoopl lab) f
    livetran n@(ASMJneNode' i lab) f = fromJust $ mapLookup (asmLabeltoHoopl lab) f
    livetran n@(ASMJgNode' i  lab) f = fromJust $ mapLookup (asmLabeltoHoopl lab) f
    livetran n@(ASMJgeNode' i lab) f = fromJust $ mapLookup (asmLabeltoHoopl lab) f
    livetran n@(ASMJlNode' i  lab) f = fromJust $ mapLookup (asmLabeltoHoopl lab) f
    livetran n@(ASMJleNode' i lab) f = fromJust $ mapLookup (asmLabeltoHoopl lab) f
    livetran n@(ASMRetNode'   i)     f = bottom
    livetran n@(ASMCallNode'  i sym) f = bottom 
    -- not sure about these yet CHANGE




    -- delete doesn't actually need n
    opdelete, opinsert :: ASMOperand -> ASMNode' e x -> VarFact -> VarFact
    opdelete (ASMRegOperand r i) n = S.filter (\(x,y) -> (x /= r))
    -- only inserts registers
    opinsert (ASMRegOperand r i) n = S.insert (r,asmDropPrime n)
    opinsert _ _ = id

    opdelete' r n = S.filter (\(x,y) -> (x /= r))
    -- only inserts registers
    opinsert' r n = S.insert (r,asmDropPrime n)

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
rewrite :: (FuelMonad m) => BwdRewrite m ASMNode' VarFact
rewrite = deepBwdRw (\n f -> return Nothing)

-- define lattice
bottom = S.empty
lattice = DataflowLattice
  { fact_name = "ASM Live Variables"
  , fact_bot  = bottom
  , fact_join = join }

-- define fwd pass
asmLiveVars  = BwdPass
  { bp_lattice  = lattice
  , bp_transfer = transfer
  , bp_rewrite  = rewrite }
