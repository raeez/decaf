{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, NoMonomorphismRestriction #-}

module Decaf.Passes.Copy where
import qualified Data.Map as Map
import Decaf.IR.LIR
import Decaf.IR.IRNode
import Control.Monad
import Decaf.LIRNodes
import Loligoptl.Combinators
import Debug.Trace
import Data.Maybe
import Loligoptl

import qualified Data.Map as M
----------------------------------------------------

data CSEKey = CSEKey LIROperand LIRBinOp LIROperand
              deriving (Show, Ord)

instance Eq CSEKey where
    (==) = keyEq 

keyEq :: CSEKey -> CSEKey -> Bool               -- FIX ME: an expression is equal iff operation is commutative  (SIMPLIFICATION; NOT ALWAYS TRUE)
keyEq (CSEKey op1 binop op2) (CSEKey op1' binop' op2')
    | (binop == binop') && (op1 == op1') && (op2 == op2') = True
    | otherwise                                           = False
    -- | (binop == binop') && (op1 == op2') && (op2 == op1') = True

data CSEData = CSETop
             | CSEReg LIRReg
             deriving (Show, Eq)

data CSEFact = CSEFactMap (M.Map CSEKey CSEData)
             | CSEBot
             deriving (Show, Eq)

-- join two CSE facts 
joinCSEFact :: JoinFun CSEFact
joinCSEFact l (OldFact x) (NewFact CSEBot) = (NoChange, x)
joinCSEFact l (OldFact CSEBot) (NewFact y) = (SomeChange, y)
joinCSEFact l (OldFact (CSEFactMap x)) (NewFact (CSEFactMap y)) = 
    extract (M.intersectionWith (\x y -> join (unconv x) (unconv y)) (M.map convertUnchanged x) (M.map convertChanged y))
  where 
    convertChanged, convertUnchanged :: CSEData -> (ChangeFlag, CSEData)
    convertChanged x = (SomeChange, x)
    convertUnchanged x = (NoChange, x)
    unconv :: (ChangeFlag, CSEData) -> CSEData
    unconv (_, x) = x
    join :: CSEData -> CSEData -> (ChangeFlag, CSEData)
    join CSETop CSETop = (NoChange,   CSETop)
    join x CSETop = (SomeChange, CSETop)
    join CSETop x = (NoChange,   CSETop)
    join x y      = if x == y
                      then (NoChange, x)
                      else (SomeChange, CSETop)
    extract :: M.Map CSEKey (ChangeFlag, CSEData) -> (ChangeFlag, CSEFact)
    extract x =  ( if changed x then SomeChange else NoChange
                 , CSEFactMap $ M.map snd x)
    changed :: M.Map CSEKey (ChangeFlag, CSEData) -> Bool
    changed x = or $ map snd (M.toList (M.map ((== SomeChange) . fst) x))

-- define const lattice
cseBottom = fact_bot cseLattice
cseTop = CSEFactMap $ M.empty
cseLattice :: DataflowLattice CSEFact
cseLattice = DataflowLattice
  { fact_name = "CommonSubexpressionMap"
  , fact_bot  = CSEBot
  , fact_join = joinCSEFact }

-- aux: remvoe all records containing x, because x has been changed
varChanged :: M.Map CSEKey CSEData -> LIRReg -> M.Map CSEKey CSEData
--varChanged CSEBot x = error "varChanged: called on CSEBot; should not happen!"
varChanged f x = M.mapWithKey keep f
  where 
    -- a record is ok (to keep) if it does not contain x
    keep :: CSEKey -> CSEData -> CSEData
    keep (CSEKey a op b) (CSEReg c) = if (oprMismatch a) && (oprMismatch b) && (c /= x)
                                      then CSEReg c
                                      else CSETop
    keep (CSEKey a op b) CSETop = CSETop
    keep key val = val
                     
    -- a operand does not contains x if it is not a RegOperand made with x
    oprMismatch :: LIROperand -> Bool
    -- if it is a RegOperand, then change x /= x'
    oprMismatch (LIRRegOperand x') = (x /= x')  
    -- mismatch always holds otherwise
    oprMismatch _                  = True

-- transfer: define CSE lattice transfer function:  a expression (x op y) is available iff ((x,op,y),w) \in Lattice
-- concerns only RegAssignNode and LoadNode
cseTransfer :: FwdTransfer LIRNode CSEFact
cseTransfer = mkFTransfer unwrapFactFt
  where
    unwrapFactFt :: LIRNode e x -> CSEFact -> Fact x CSEFact
    unwrapFactFt n CSEBot = error "cseTransfer:unwrapFactFt called on CSEBot; should not happen!"
    unwrapFactFt n (CSEFactMap f) = ft n f

    ft :: LIRNode e x -> M.Map CSEKey CSEData -> Fact x CSEFact
    ft (LIRLabelNode {}) f         = CSEFactMap f
    ft (LIRRegAssignNode x (LIRBinExpr a op b)) f
                                   = CSEFactMap $ M.insert (CSEKey a op b) (CSEReg x) (varChanged f x) -- a op b -> x
    ft (LIRRegAssignNode x _) f    = CSEFactMap $ varChanged f x                                       -- x = _, no change
    ft (LIRRegOffAssignNode {}) f  = CSEFactMap $ f                                       -- write to a memory location indexed by the registers
    ft (LIRStoreNode {}) f         = CSEFactMap $ f
    ft (LIRLoadNode x _) f         = CSEFactMap $ varChanged f x                          -- remove all records containing x
    ft (LIRCallNode proc ret) f    = mkFactBase cseLattice [(proc, CSEFactMap f), (ret, CSEFactMap mkGlobalsTop)] -- remove all local vars (i.e. only keep global vars) when jumping to the function
      where
        mkGlobalsTop = M.mapWithKey globalToTop f
        globalToTop (CSEKey a op b) (CSEReg (SREG s)) = if local a && local b
                                                          then CSEReg (SREG s)
                                                          else CSETop
        globalToTop _ _ = CSETop
        local (LIRRegOperand (SREG {})) = True
        local (LIRIntOperand {}) = True
        local _ = False

    ft (LIRCalloutNode {}) f       = CSEFactMap f
    ft (LIREnterNode {}) f         = CSEFactMap f
    ft (LIRRetNode successors _) f = mkFactBase cseLattice [] -- (map (\x -> (x, f)) successors)
    ft (LIRIfNode expr tl fl) f    = mkFactBase cseLattice [(tl, CSEFactMap f), (fl, CSEFactMap f)]  -- if expr the jmp tl else jmp fl
    ft (LIRJumpLabelNode l) f      = mkFactBase cseLattice [(l, CSEFactMap f)]        -- jmp l --> associate f with l 

-- rewrite: define constant folding rewrites
cseRewrite :: FuelMonad m => FwdRewrite m LIRNode CSEFact
cseRewrite  = shallowFwdRw simp
  where
    simp :: forall m e x . (ShapeLifter e x, FuelMonad m) => 
            LIRNode e x -> CSEFact -> m (Maybe (Graph LIRNode e x))
    simp node CSEBot = error "cse: called on CSEBot; should not happen!"
    -- simp node (CSEFactMap f) = return $ liftM nodeToG $ s_node (trace ("REWRITING NODE [" ++ show node ++ "] ~~~~~~~~~WITH FACTS~~~~~~~~~~~~ {\n" ++ unlines(map show $ M.toList f) ++ "}") node)
    simp node (CSEFactMap f) = return $ liftM nodeToG $ s_node node
  
      where
        s_node :: LIRNode e x -> Maybe (LIRNode e x)
        -- x = lit op lit     
        s_node (LIRRegAssignNode reg (LIRBinExpr o1 binop o2)) = 
          case M.lookup (CSEKey o1 binop o2) f of
            -- Just (CSEReg result)  -> trace ("lookup found! rewrote to " ++ show (LIRRegAssignNode reg $ LIROperExpr $ LIRRegOperand result)) (Just $ LIRRegAssignNode reg $ LIROperExpr $ LIRRegOperand result)
            Just (CSEReg result)  -> Just $ LIRRegAssignNode reg $ LIROperExpr $ LIRRegOperand result
            -- Just CSETop           -> trace "lookup failed!" Nothing
            Just CSETop           -> Nothing
            Nothing -> Nothing

        -- others do not need CSE
        s_node _ = Nothing
-- define fwd pass
-- csePass = FuelMonad m => FwdPass m 
csePass  = FwdPass 
  { fp_lattice = cseLattice
  , fp_transfer = cseTransfer
  , fp_rewrite = cseRewrite
  }
