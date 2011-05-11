{-# OPTIONS_GHC -Wall -fno-warn-incomplete-patterns #-}
{-# LANGUAGE GADTs, NoMonomorphismRestriction,ScopedTypeVariables, GADTs #-}
module Decaf.Passes.Live (liveLattice, liveness, deadAsstElim, livePass) where

import Decaf.IR.LIR
import Decaf.IR.IRNode
import Control.Monad
import Decaf.LIRNodes
import Loligoptl
import Debug.Trace
import Data.Maybe
import Loligoptl
import qualified Data.Set as S

type LiveFact = S.Set LIRReg
liveLattice :: DataflowLattice LiveFact
liveLattice = DataflowLattice
  { fact_name = "LiveFact variables"
  , fact_bot  = S.empty
  , fact_join = add
  }
    where add _ (OldFact old) (NewFact new) = (ch, j)
            where
              j = new `S.union` old
              ch = changeIf (S.size j > S.size old)

liveness :: BwdTransfer LIRNode LiveFact
liveness = mkBTransfer live
  where
    live :: LIRNode e x -> Fact x LiveFact -> LiveFact
    live n@(LIRLabelNode _) f          = f
    live n@(LIRRegAssignNode x _) f    = addUses (S.delete x f) n
    live n@(LIRRegOffAssignNode x _ _ _) f
                                       = addUses f n
    live n@(LIRStoreNode {}) f         = addUses f n
    live n@(LIRLoadNode x _) f         = addUses (S.delete x f) n
    live n@(LIRCallNode proc ret) f    = addUses (fact f proc) n
    live n@(LIRCalloutNode {}) f       = f
    live n@(LIREnterNode {}) f         = f
    live n@(LIRRetNode successors _) f = addUses (fact_bot liveLattice) n
    live n@(LIRIfNode expr tl fl) f    = addUses (fact f tl `S.union` fact f fl) n
    live n@(LIRJumpLabelNode l) f      = addUses (fact f l) n

    fact :: FactBase (S.Set LIRReg) -> Label -> LiveFact
    fact f l = fromMaybe S.empty (mapLookup l f)
    
    addUses :: S.Set LIRReg -> LIRNode e x -> LiveFact
    addUses f (LIRLabelNode _)              = f
    addUses f (LIRRegAssignNode x e)        = trackExpr f e
    addUses f (LIRRegOffAssignNode _ _ _ o) = trackOperand f o
    addUses f (LIRStoreNode _ o)            = trackOperand f o
    addUses f (LIRLoadNode r _)             = f
    addUses f (LIRCallNode _ _)             = f
    addUses f (LIRCalloutNode _)            = f
    addUses f (LIREnterNode _)              = f
    addUses f (LIRRetNode _ _)              = f
    addUses f (LIRIfNode _ _ _)             = f
    addUses f (LIRJumpLabelNode _)          = f

    trackExpr :: S.Set LIRReg -> LIRExpr -> S.Set LIRReg
    trackExpr f (LIRBinExpr o1 _ o2) = trackOperand (trackOperand f o1) o2
    trackExpr f (LIRUnExpr _ o)      = trackOperand f o
    trackExpr f (LIROperExpr o)      = trackOperand f o

    trackOperand :: S.Set LIRReg -> LIROperand -> S.Set LIRReg
    trackOperand f (LIRRegOperand r@(SREG {})) = trackRegister f r
    trackOperand f (LIRRegOperand r@(MEM {})) = trackRegister f r
    trackOperand f (LIRRegOperand r@(GI {})) = trackRegister f r
    trackOperand f _                 = f

    trackRegister :: S.Set LIRReg -> LIRReg -> S.Set LIRReg
    trackRegister f r = trace ("tracking register[" ++ pp r ++ "]") $ S.insert r f


-- deadAsstElim :: forall m . FuelMonad m => BwdRewrite m LIRNode LiveFact
deadAsstElim :: FuelMonad m => BwdRewrite m LIRNode LiveFact
deadAsstElim = mkBRewrite d
  where
    d :: forall e x m. FuelMonad m => LIRNode e x -> Fact x LiveFact -> m (Maybe (Graph LIRNode e x))
    -- d n@(LIRRegAssignNode x@(SREG {}) _) live
        -- | not (x `S.member` live) = trace ("rewriting " ++ show n ++ " with fact: " ++ show live) $ return $ Just GNil

    -- d n@(LIRRegAssignNode x@(MEM {}) _) live
        -- | not (x `S.member` live) = trace ("rewriting " ++ show n ++ " with fact: " ++ show live) $ return $ Just GNil

    -- d n@(LIRRegAssignNode x@(GI {}) _) live
        -- | not (x `S.member` live) = trace ("rewriting " ++ show n ++ " with fact: " ++ show live) $ return $ Just GNil

    -- d n@(LIRLoadNode x@(SREG {}) _) live
        -- | not (x `S.member` live) = trace ("rewriting " ++ show n ++ " with fact: " ++ show live) $ return $ Just GNil

    -- d n@(LIRLoadNode x@(MEM {}) _) live
        -- | not (x `S.member` live) = trace ("rewriting " ++ show n ++ " with fact: " ++ show live) $ return $ Just GNil

    -- d n@(LIRLoadNode x@(GI {}) _) live
        -- | not (x `S.member` live) = trace ("rewriting " ++ show n ++ " with fact: " ++ show live) $ return $ Just GNil
    d _ _ = return Nothing

-- livePass :: (NonLocal n, Monad m) => BwdPass m n LiveFact
livePass = BwdPass
    { bp_lattice = liveLattice
    , bp_transfer = liveness
    , bp_rewrite = deadAsstElim
    }
