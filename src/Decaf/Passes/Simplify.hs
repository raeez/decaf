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
    s_node (LIRIfNode e fl tl) = let e' = rewriteRelExpr e
                                     e'' = trace ("collapsing branch with expr: " ++ pp e') e'
                                 in case e'' of
                                    (LIRNotRelExpr (LIRIntOperand i))
                                        -> Just $ LIRJumpLabelNode (if i == asmFalse
                                                                      then trace ("rel: picked true branch: " ++ pp tl) tl
                                                                      else if i == asmTrue
                                                                        then trace ("rel: picked false branch: " ++ pp fl) fl
                                                                        else error $ "tried to collapse an if node with an invalid relexpr of value: " ++ pp i)

                                    (LIROperRelExpr (LIRIntOperand i))
                                        -> Just $ LIRJumpLabelNode (if i == asmTrue
                                                                      then trace ("rel: picked true branch: " ++ pp tl) tl
                                                                      else if i == asmFalse
                                                                        then trace ("rel: picked false branch: " ++ pp fl) fl
                                                                        else error $ "tried to collapse an if node with an invalid relexpr of value: " ++ pp i)

                                    _   -> trace "FAIL: could not collapse branch" $ Just $ LIRIfNode e' fl tl
    s_node _ = Nothing
    unOp op  = case op of
            LNEG -> (-) 0
            LNOT -> toLIRBool . not . toHaskBool

    binOp :: LIRBinOp -> LIRInt -> LIRInt -> LIRInt
    binOp op o1 o2 = case op of
            LADD -> (+) o1 o2
            LSUB -> (-) o1 o2
            LMUL -> (*) o1 o2
            LDIV -> div o1 o2
            LMOD -> mod o1 o2
            LAND -> toLIRBool $ (&&) (toHaskBool o1) (toHaskBool o2)
            LOR ->  toLIRBool $ (||) (toHaskBool o1) (toHaskBool o2)
            LIRBinRelOp op' _ ->  trace ("redirecting to relBinOp") relBinOp op' o1 o2

    relBinOp :: LIRRelOp -> LIRInt -> LIRInt -> LIRInt
    relBinOp op o1 o2 = trace ("called relBinOp with " ++ pp op) $ case op of
            LEQ ->  toLIRBool $ trace ("called EQ than on " ++ pp o1 ++ " vs " ++ pp o2)  $ (==) o1 o2
            LNEQ -> toLIRBool $ trace ("called NEQ than on " ++ pp o1 ++ " vs " ++ pp o2) $ (/=) o1 o2
            LGT ->  toLIRBool $ trace ("called GT than on " ++ pp o1 ++ " vs " ++ pp o2)  $ (>) o1 o2
            LGTE -> toLIRBool $ trace ("called GTE than on " ++ pp o1 ++ " vs " ++ pp o2) $(>=) o1 o2
            LLT ->  toLIRBool $ trace ("called LT than on " ++ pp o1 ++ " vs " ++ pp o2)  $(<) o1 o2
            LLTE -> toLIRBool $ trace ("called LTE than on " ++ pp o1 ++ " vs " ++ pp o2) $(<=) o1 o2


    toHaskBool i = if i == asmTrue
                    then True
                    else False

    toLIRBool b = if b
                    then trace ("returning asmTrue") asmTrue
                    else trace ("returning asmFalse") asmFalse

    rewriteExpr a@(LIRBinExpr (LIRIntOperand i) op (LIRIntOperand i')) =
        let target = LIROperExpr $ LIRIntOperand $ binOp op i i'
        in trace ("binexpr; rewrote " ++ pp a ++ " to " ++ pp target) target
    rewriteExpr (LIRBinExpr o1 op o2) = LIRBinExpr o1 op o2
    rewriteExpr (LIRUnExpr op (LIRIntOperand i)) =
        LIROperExpr $ LIRIntOperand $ (unOp op) i
    rewriteExpr (LIRUnExpr a o)      = LIRUnExpr a o
    rewriteExpr (LIROperExpr o)      = LIROperExpr o


    rewriteRelExpr a@(LIRBinRelExpr (LIRIntOperand i) op (LIRIntOperand i')) =
        let target = LIROperRelExpr $ LIRIntOperand $ relBinOp op i i'
        in trace ("binrelexpr: rewrote " ++ pp a ++ " to " ++ pp target) target
    rewriteRelExpr (LIRBinRelExpr o1 a o2) = LIRBinRelExpr o1 a o2
    rewriteRelExpr a@(LIRNotRelExpr (LIRIntOperand i)) =
        LIROperRelExpr $ LIRIntOperand $ let target = if i == asmTrue
                                                        then asmFalse
                                                        else asmTrue
                                         in trace ("notrelexpr: rewrote " ++ pp a ++ " to " ++ pp target) target
    rewriteRelExpr (LIRNotRelExpr o)       = LIRNotRelExpr o
    rewriteRelExpr (LIROperRelExpr o)      = LIROperRelExpr o
