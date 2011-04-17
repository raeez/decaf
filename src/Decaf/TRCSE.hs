{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, NoMonomorphismRestriction #-}

module Decaf.TRCSE where
import qualified Data.Map as Map
import Decaf.IR.LIR
import Decaf.IR.IRNode
import Control.Monad
import Decaf.HooplNodes
import Loligoptl.Dataflow
import Loligoptl.Graph 
import Loligoptl.Fuel 
import Loligoptl.Label 
import Loligoptl.Combinators
import Data.Maybe

import qualified Data.Map as M
----------------------------------------------------
-- this file contains CSE transfer/rewrite functions
-- currently it is local rewrite,  
-- would need to expand to global rewrite later

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

type CSEFact = M.Map CSEKey CSEData

-- join two CSE facts 
joinCSEFact :: CSEFact -> CSEFact -> (ChangeFlag, CSEFact)
joinCSEFact x y = 
    extract (M.unionWith (\x y -> join (unconv x) (unconv y)) (M.map convertUnchanged x) (M.map convertChanged y))
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
    extract :: M.Map CSEKey (ChangeFlag, CSEData) -> (ChangeFlag, M.Map CSEKey CSEData)
    extract x =  ( if changed x then SomeChange else NoChange
                 , M.map snd x)
    changed :: M.Map CSEKey (ChangeFlag, CSEData) -> Bool
    changed x = or $ map snd (M.toList (M.map ((== SomeChange) . fst) x))

-- define const lattice
cseBottom = factBottom cseLattice
cseLattice :: DataflowLattice CSEFact
cseLattice = DataflowLattice
  { factBottom = M.empty
  , factJoin   = joinCSEFact }

-- aux: remvoe all records containing x, because x has been changed
varChanged :: CSEFact -> LIRReg -> CSEFact
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
exprIsAvail :: FwdTransfer Node CSEFact
exprIsAvail = mkFTransfer ft
  where
    ft :: Node e x -> CSEFact -> Fact x CSEFact
    ft (LIRLabelNode {}) f         = f
    ft (LIRRegAssignNode x (LIRBinExpr a op b)) f
                                   = M.insert (CSEKey a op b) (CSEReg x) (varChanged f x) -- a op b -> x
    ft (LIRRegAssignNode x _) f    = varChanged f x                                       -- x = _, no change
    ft (LIRRegOffAssignNode {}) f  = f                                       -- write to a memory location indexed by the registers
    ft (LIRStoreNode {}) f         = f
    ft (LIRLoadNode x _) f         = varChanged f x                          -- remove all records containing x
    ft (LIRCallNode proc ret) f    = mkFactBase cseLattice [(proc, f), (ret, mkGlobalsTop)] -- remove all local vars (i.e. only keep global vars) when jumping to the function
      where
        mkGlobalsTop = M.mapWithKey globalToTop f
        globalToTop (CSEKey a op b) (CSEReg (SREG s)) = if local a && local b
                                                          then CSEReg (SREG s)
                                                          else CSETop
        globalToTop _ _ = CSETop
        local (LIRRegOperand (SREG {})) = True
        local (LIRIntOperand {}) = True
        local _ = False

    ft (LIRCalloutNode {}) f       = f
    ft (LIREnterNode {}) f         = f
    ft (LIRRetNode successors _) f = mkFactBase cseLattice [] -- (map (\x -> (x, f)) successors)
    ft (LIRIfNode expr tl fl) f    = mkFactBase cseLattice [(tl, f), (fl, f)]  -- if expr the jmp tl else jmp fl
    ft (LIRJumpLabelNode l) f      = mkFactBase cseLattice [(l, f)]        -- jmp l --> associate f with l 

-- rewrite: define constant folding rewrites
cse :: Monad m => FwdRewrite m Node CSEFact
cse  = shallowFwdRw simp
  where
    simp :: forall m e x . (ShapeLifter e x, Monad m) => 
            Node e x -> CSEFact -> m (Maybe (Graph Node e x))
    simp node f = return $ liftM nodeToG $ s_node node
  
      where
        s_node :: Node e x -> Maybe (Node e x)
        -- x = lit op lit     
        s_node (LIRRegAssignNode reg (LIRBinExpr o1 binop o2)) = 
          case M.lookup (CSEKey o1 binop o2) f of
            Just (CSEReg result)  -> Just $ LIRRegAssignNode reg $ LIROperExpr $ LIRRegOperand result
            Just CSETop           -> Nothing
            Nothing -> Nothing

        -- others do not need CSE
        s_node _ = Nothing
-- define fwd pass
csePass  = FwdPass 
  { fpLattice = cseLattice
  , fpTransfer = exprIsAvail
  , fpRewrite = cse }
