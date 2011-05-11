{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables, GADTs #-}
module Decaf.Passes.Constant (ConstFact, constTop, constLattice, constPass) where

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

type ConstKey = LIRReg

data ConstData = ConstTop
               | ConstLit LIRInt
               deriving (Show, Eq)

data ConstFact = ConstFactMap (M.Map ConstKey ConstData)
               | ConstBot
                deriving (Show, Eq)

constBottom = fact_bot constLattice
constTop = ConstFactMap $ M.empty
constLattice :: DataflowLattice ConstFact
constLattice = DataflowLattice
 { fact_name = "Constant Propogation"
 , fact_bot  = ConstBot
 , fact_join = joinConstFact
 }

-- join two Const facts 
joinConstFact :: JoinFun ConstFact
joinConstFact l (OldFact x) (NewFact ConstBot) = (NoChange, x)
joinConstFact l (OldFact ConstBot) (NewFact y) = (SomeChange, y)
joinConstFact l (OldFact (ConstFactMap x)) (NewFact (ConstFactMap y)) = 
    extract (M.intersectionWith (\x y -> join (unconv x) (unconv y)) (M.map convertUnchanged x) (M.map convertChanged y))
  where 
    convertChanged, convertUnchanged :: ConstData -> (ChangeFlag, ConstData)
    convertChanged x = (SomeChange, x)
    convertUnchanged x = (NoChange, x)
    unconv :: (ChangeFlag, ConstData) -> ConstData
    unconv (_, x) = x
    join :: ConstData -> ConstData -> (ChangeFlag, ConstData)
    join ConstTop ConstTop = (NoChange,   ConstTop)
    join x ConstTop = (SomeChange, ConstTop)
    join ConstTop x = (NoChange,   ConstTop)
    join x y      = if x == y
                      then (NoChange, x)
                      else (SomeChange, ConstTop)
    extract :: M.Map ConstKey (ChangeFlag, ConstData) -> (ChangeFlag, ConstFact)
    extract x =  ( if changed x then SomeChange else NoChange
                 , ConstFactMap $ M.map snd x)
    changed :: M.Map ConstKey (ChangeFlag, ConstData) -> Bool
    changed x = or $ map snd (M.toList (M.map ((== SomeChange) . fst) x))

varChanged :: M.Map ConstKey ConstData -> LIRReg -> M.Map ConstKey ConstData
--varChanged ConstBot x = error "varChanged: called on ConstBot; should not happen!"
varChanged f x = M.insert x ConstTop f

--------------------------------------------------
-- Analysis: variable equals a literal constant
constTransfer :: FwdTransfer LIRNode ConstFact
constTransfer = mkFTransfer unwrapFactFt
 where
    unwrapFactFt :: LIRNode e x -> ConstFact -> Fact x ConstFact
    unwrapFactFt n ConstBot = error "constTransfer:unwrapFactFt called on ConstBot; should not happen!"
    unwrapFactFt n (ConstFactMap f) = ft n f

    ft :: LIRNode e x -> M.Map ConstKey ConstData -> Fact x ConstFact
    ft (LIRLabelNode {}) f         = ConstFactMap f
    ft (LIRRegAssignNode x (LIROperExpr (LIRIntOperand i))) f
                                   = ConstFactMap $ M.insert x (ConstLit i) (varChanged f x) -- a op b -> x
    ft (LIRRegAssignNode x _) f    = ConstFactMap $ varChanged f x                                       -- x = _, no change
    ft (LIRRegOffAssignNode {}) f  = ConstFactMap $ f                                       -- write to a memory location indexed by the registers
    ft (LIRStoreNode {}) f         = ConstFactMap $ f
    ft (LIRLoadNode x _) f         = ConstFactMap $ varChanged f x                          -- remove all records containing x
    ft (LIRCallNode proc ret) f    = mkFactBase constLattice [(proc, ConstFactMap f), (ret, ConstFactMap mkGlobalsTop)] -- remove all local vars (i.e. only keep global vars) when jumping to the function
      where
        mkGlobalsTop = M.mapWithKey globalToTop f
        globalToTop r i = if local r
                            then i
                            else ConstTop
        globalToTop _ _ = ConstTop
        local (SREG {}) = True
        local _ = False

    ft (LIRCalloutNode {}) f       = ConstFactMap f
    ft (LIREnterNode {}) f         = ConstFactMap f
    ft (LIRRetNode successors _) f = mkFactBase constLattice [] -- (map (\x -> (x, f)) successors)
    ft (LIRIfNode expr fl tl) f    = mkFactBase constLattice [(tl, ConstFactMap f), (fl, ConstFactMap f)]  -- if expr the jmp tl else jmp fl
    ft (LIRJumpLabelNode l) f      = mkFactBase constLattice [(l, ConstFactMap f)]        -- jmp l --> associate f with l 

constRewrite :: forall m. FuelMonad m => FwdRewrite m LIRNode ConstFact
constRewrite = mkFRewrite cp
 where
    cp :: forall m e x . (ShapeLifter e x, FuelMonad m) =>
            LIRNode e x -> ConstFact -> m (Maybe (Graph LIRNode e x))
    cp node ConstBot = error "cp: called on ConstTop; should not happen!"
    cp node (ConstFactMap f) = return $ liftM nodeToG $ s_node node
      where
        s_node :: LIRNode e x -> Maybe (LIRNode e x)
        -- x = lit op lit     
        s_node (LIRRegAssignNode x e) = Just $ LIRRegAssignNode x (rewriteExpr e)
        s_node (LIRRegOffAssignNode a b c o) = Just $ LIRRegOffAssignNode a b c (rewriteOperand o)
        s_node (LIRStoreNode a o) = Just $ LIRStoreNode a (rewriteOperand o)
        s_node (LIRIfNode e a b) = Just $ LIRIfNode (rewriteRelExpr e) a b
        s_node _ = Nothing

        rewriteExpr (LIRBinExpr o1 a o2) = LIRBinExpr (rewriteOperand o1) a (rewriteOperand o2)
        rewriteExpr (LIRUnExpr a o)      = LIRUnExpr a (rewriteOperand o)
        rewriteExpr (LIROperExpr o)      = LIROperExpr (rewriteOperand o)

        rewriteRelExpr (LIRBinRelExpr o1 a o2) = LIRBinRelExpr (rewriteOperand o1) a (rewriteOperand o2)
        rewriteRelExpr (LIRNotRelExpr o)     = LIRNotRelExpr (rewriteOperand o)
        rewriteRelExpr (LIROperRelExpr o)      = LIROperRelExpr (rewriteOperand o)

        rewriteOperand (LIRRegOperand r) = constLook r
        rewriteOperand a = a

        constLook r =
          case M.lookup r f of
            Just (ConstLit i) -> trace ("rewriting register " ++ pp r ++ " with constant " ++ pp i) (LIRIntOperand i)
            Just ConstTop       -> LIRRegOperand r
            Nothing -> LIRRegOperand r

constPass  = FwdPass 
  { fp_lattice = constLattice
  , fp_transfer = constTransfer
  , fp_rewrite = constRewrite `thenFwdRw` simplify
  }
