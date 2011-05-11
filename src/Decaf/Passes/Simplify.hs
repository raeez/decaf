{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables, GADTs, PatternGuards #-}
module Decaf.Passes.Simplify (simplify) where

import Control.Monad
import qualified Data.Map as Map
import Decaf.IR.LIR
import Decaf.IR.IRNode
import Control.Monad
import Decaf.LIRNodes
import Loligoptl.Combinators
import Debug.Trace
import Data.Maybe
import Data.Int
import Loligoptl

--------------------------------------------------
-- Simplification ("constant folding")
simplify :: forall m f. FuelMonad m => FwdRewrite m LIRNode f
simplify = mkFRewrite simp
 where
    simp :: forall m e x f. (ShapeLifter e x, FuelMonad m) =>
            LIRNode e x -> f -> m (Maybe (Graph LIRNode e x))
    simp node _ = return $ liftM nodeToG $ s_node node
    s_node :: LIRNode e x -> Maybe (LIRNode e x)
    s_node (LIRRegAssignNode x e)        = Just $ LIRRegAssignNode x (rewriteExpr e)
    s_node (LIRIfNode e tl fl) = let e' = rewriteRelExpr e
                                 in case e' of
                                    (LIRNotRelExpr (LIRIntOperand i))
                                        -> Just $ LIRJumpLabelNode (if i == asmFalse then tl else fl) 

                                    (LIROperRelExpr (LIRIntOperand i))
                                        -> Just $ LIRJumpLabelNode (if i == asmTrue then tl else fl)

                                    _   -> Just $ LIRIfNode e' tl fl
    s_node _ = Nothing
    unOp op  = case op of
            LNEG -> (-) 0
            LNOT -> toLIRBool . not . toHaskBool

    -- binOp :: forall i. Fractional i => LIRBinOp -> i -> i
    binOp op = case op of
            LADD -> (+)
            LSUB -> (-)
            LMUL -> (*)
            LDIV -> div
            LMOD -> mod
            LAND -> liftedOp (&&)
            LOR -> liftedOp (||)
            LIRBinRelOp LEQ _ -> liftedOp (==) 
            LIRBinRelOp LNEQ _ -> liftedOp (/=)
            LIRBinRelOp LGT _ -> liftedOp (>)
            LIRBinRelOp LGTE _ -> liftedOp (>=)
            LIRBinRelOp LLT _ -> liftedOp (<)
            LIRBinRelOp LLTE _ -> liftedOp (<=)

    relBinOp op = case op of
            LEQ -> (==)
            LNEQ -> (/=)
            LGT -> (>)
            LGTE -> (>=)
            LLT -> (<)
            LLTE -> (<=)

    liftedOp op i i' = toLIRBool $ (toHaskBool i) `op` (toHaskBool i')

    toHaskBool i = if i == asmTrue
                    then True
                    else False
    toLIRBool b = if True
                    then asmTrue
                    else asmFalse

    rewriteExpr (LIRBinExpr (LIRIntOperand i) op (LIRIntOperand i')) =
        LIROperExpr $ LIRIntOperand $ (binOp op) i i'
    rewriteExpr (LIRBinExpr o1 op o2) = LIRBinExpr o1 op o2
    rewriteExpr (LIRUnExpr op (LIRIntOperand i)) =
        LIROperExpr $ LIRIntOperand $ (unOp op) i
    rewriteExpr (LIRUnExpr a o)      = LIRUnExpr a o
    rewriteExpr (LIROperExpr o)      = LIROperExpr o


    rewriteRelExpr (LIRBinRelExpr (LIRIntOperand i) op (LIRIntOperand i')) =
        LIROperRelExpr $ LIRIntOperand $ toLIRBool $ (relBinOp op) (toHaskBool i) (toHaskBool i')
    rewriteRelExpr (LIRBinRelExpr o1 a o2) = LIRBinRelExpr o1 a o2
    rewriteRelExpr (LIRNotRelExpr (LIRIntOperand i)) =
        LIROperRelExpr $ LIRIntOperand $ if i == asmTrue
                                            then asmFalse
                                            else asmTrue
    rewriteRelExpr (LIRNotRelExpr o)       = LIRNotRelExpr o
    rewriteRelExpr (LIROperRelExpr o)      = LIROperRelExpr o
