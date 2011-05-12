{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables, GADTs #-}
module Decaf.Passes.Copy (CopyFact, copyTop, copyLattice, copyPass) where

import Control.Monad
import qualified Data.Map as M
import Decaf.IR.LIR
import Decaf.IR.IRNode
import Decaf.Passes.Simplify
import Control.Monad
import Decaf.LIRNodes
import Loligoptl.Combinators
import Debug.Trace
import Data.Maybe
import Loligoptl

type CopyKey = LIRReg

data CopyData = CopyTop
              | CopyReg LIRReg
              deriving (Show, Eq)

data CopyFact = CopyFactMap (M.Map CopyKey CopyData)
              | CopyBot
               deriving (Show, Eq)

copyBot = fact_bot copyLattice
copyTop = CopyFactMap $ M.empty
copyLattice :: DataflowLattice CopyFact
copyLattice = DataflowLattice
 { fact_name = "Copy Propogation"
 , fact_bot  = CopyBot
 , fact_join = joinCopyFact
 }

-- join two Copy facts 
joinCopyFact :: JoinFun CopyFact
joinCopyFact l (OldFact x) (NewFact CopyBot) = (NoChange, x)
joinCopyFact l (OldFact CopyBot) (NewFact y) = (SomeChange, y)
joinCopyFact l (OldFact (CopyFactMap x)) (NewFact (CopyFactMap y)) = 
    extract (M.intersectionWith (\x y -> join (unconv x) (unconv y)) (M.map convertUnchanged x) (M.map convertChanged y))
  where 
    convertChanged, convertUnchanged :: CopyData -> (ChangeFlag, CopyData)
    convertChanged x = (SomeChange, x)
    convertUnchanged x = (NoChange, x)
    unconv :: (ChangeFlag, CopyData) -> CopyData
    unconv (_, x) = x
    join :: CopyData -> CopyData -> (ChangeFlag, CopyData)
    join CopyTop CopyTop = (NoChange,   CopyTop)
    join x CopyTop = (SomeChange, CopyTop)
    join CopyTop x = (NoChange,   CopyTop)
    join x y      = if x == y
                      then (NoChange, x)
                      else (SomeChange, CopyTop)
    extract :: M.Map CopyKey (ChangeFlag, CopyData) -> (ChangeFlag, CopyFact)
    extract x =  ( if changed x then SomeChange else NoChange
                 , CopyFactMap $ M.map snd x)
    changed :: M.Map CopyKey (ChangeFlag, CopyData) -> Bool
    changed x = or $ map snd (M.toList (M.map ((== SomeChange) . fst) x))

varChanged :: M.Map CopyKey CopyData -> LIRReg -> M.Map CopyKey CopyData
varChanged f x = M.insert x CopyTop f

--------------------------------------------------
-- Analysis: variable equals a literal constant
copyTransfer :: FwdTransfer LIRNode CopyFact
copyTransfer = mkFTransfer unwrapFactFt
 where
    unwrapFactFt :: LIRNode e x -> CopyFact -> Fact x CopyFact
    unwrapFactFt n CopyBot = error "constTransfer:unwrapFactFt called on CopyBot; should not happen!"
    unwrapFactFt n (CopyFactMap f) = ft n f

    ft :: LIRNode e x -> M.Map CopyKey CopyData -> Fact x CopyFact
    ft (LIRLabelNode {}) f         = CopyFactMap f
    ft (LIRRegAssignNode x (LIROperExpr (LIRRegOperand r))) f
                                   = CopyFactMap $ skipForbidden
      where
        forbidden =  [LRDI, LRAX, LRDI, LRSI, LRCX, LRDX, LR8, LR9]
        skipForbidden = if (x `notElem` forbidden) && (r `notElem` forbidden)
                          then M.insert x (CopyReg r) (varChanged f x)
                          else f
    ft (LIRRegAssignNode x _) f    = CopyFactMap $ varChanged f x                                       -- x = _, no change
    ft (LIRRegOffAssignNode {}) f  = CopyFactMap $ f                                       -- write to a memory location indexed by the registers
    ft (LIRStoreNode {}) f         = CopyFactMap $ f
    ft (LIRLoadNode x _) f         = CopyFactMap $ varChanged f x                          -- remove all records containing x
    ft (LIRCallNode proc ret) f    = mkFactBase copyLattice [(proc, CopyFactMap mkLocalsTop), (ret, CopyFactMap mkRegTop)] -- remove all local vars (i.e. only keep global vars) when jumping to the function
      where
        mkLocalsTop = M.mapWithKey f' f
          where
            f' (GI {}) v = v
            f' _ v = CopyTop

        mkRegTop = M.mapWithKey f' f
          where
            f' _ _    = CopyTop
            f' LRAX _ = CopyTop
            f' LRDI _ = CopyTop
            f' LRSI _ = CopyTop
            f' LRCX _ = CopyTop
            f' LRDX _ = CopyTop
            f' LR8 _ = CopyTop
            f' LR9 _ = CopyTop
            f' _ (CopyReg LRAX) = CopyTop
            f' _ (CopyReg LRDI) = CopyTop
            f' _ (CopyReg LRSI) = CopyTop
            f' _ (CopyReg LRCX) = CopyTop
            f' _ (CopyReg LRDX) = CopyTop
            f' _ (CopyReg LR8) = CopyTop
            f' _ (CopyReg LR9) = CopyTop
            f' _ v    = CopyTop

    ft (LIRCalloutNode {}) f       = CopyFactMap mkRAXTop
      where
        mkRAXTop = M.mapWithKey f' f
        f' LRAX _ = CopyTop
        f' _ v    = v

    ft (LIREnterNode {}) f         = CopyFactMap f
    ft (LIRRetNode successors _) f = mkFactBase copyLattice [] -- (map (\x -> (x, f)) successors)
    ft (LIRIfNode expr fl tl) f    = mkFactBase copyLattice [(tl, CopyFactMap f), (fl, CopyFactMap f)]  -- if expr the jmp tl else jmp fl
    ft (LIRJumpLabelNode l) f      = mkFactBase copyLattice [(l, CopyFactMap f)]        -- jmp l --> associate f with l 

copyRewrite :: forall m. FuelMonad m => FwdRewrite m LIRNode CopyFact
copyRewrite = mkFRewrite cp
 where
    cp :: forall m e x . (ShapeLifter e x, FuelMonad m) =>
            LIRNode e x -> CopyFact -> m (Maybe (Graph LIRNode e x))
    cp node CopyBot = error "cp: called on CopyTop; should not happen!"
    cp node (CopyFactMap f) = return $ liftM nodeToG $ s_node node
      where
        s_node :: LIRNode e x -> Maybe (LIRNode e x)
        s_node (LIRRegAssignNode x e)          = Just $ LIRRegAssignNode x (rewriteExpr e)
        s_node (LIRRegOffAssignNode r1 r2 s o) = Just $ LIRRegOffAssignNode (rewriteReg r1) (rewriteReg r2) s (rewriteOperand o)
        s_node (LIRStoreNode m o)              = Just $ LIRStoreNode (rewriteMemAddr m) (rewriteOperand o)
        s_node (LIRLoadNode r m)               = Just $ LIRLoadNode r (rewriteMemAddr m)
        s_node (LIRIfNode rel l l2)            = Just $ LIRIfNode (rewriteRelExpr rel) l l2
        s_node _                               = Nothing

        rewriteExpr (LIRBinExpr o1 a o2) = LIRBinExpr (rewriteOperand o1) a (rewriteOperand o2)
        rewriteExpr (LIRUnExpr a o)      = LIRUnExpr a (rewriteOperand o)
        rewriteExpr (LIROperExpr o)      = LIROperExpr (rewriteOperand o)

        rewriteMemAddr (LIRMemAddr r1 (Just r2) o s) = LIRMemAddr (rewriteReg r1) (Just (rewriteReg r2)) o s
        rewriteMemAddr (LIRMemAddr r1 Nothing o s)   = LIRMemAddr (rewriteReg r1) Nothing o s

        rewriteRelExpr (LIRBinRelExpr o1 a o2) = LIRBinRelExpr (rewriteOperand o1) a (rewriteOperand o2)
        rewriteRelExpr (LIRNotRelExpr o)       = LIRNotRelExpr (rewriteOperand o)
        rewriteRelExpr (LIROperRelExpr o)      = LIROperRelExpr (rewriteOperand o)

        rewriteOperand (LIRRegOperand r) = LIRRegOperand (rewriteReg r)
        rewriteOperand a = a

        rewriteReg reg =
          case M.lookup reg f of
            Just (CopyReg r) -> trace ("rewriting register " ++ pp reg ++ " with copied " ++ pp r) r
            Just CopyTop     -> reg
            Nothing ->  reg

copyPass  = FwdPass 
  { fp_lattice = copyLattice
  , fp_transfer = copyTransfer
  , fp_rewrite = copyRewrite `thenFwdRw` simplify
  }
