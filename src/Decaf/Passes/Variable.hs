{-# LANGUAGE GADTs, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module Decaf.Passes.Variable
  ( VarFact, VarMap(..), varEntry, varLattice
  , varPass
  , writeFactMap
  )
where
import Data.Maybe
import Data.List
import Loligoptl
import Loligoptl.Label
import Decaf.IR.LIR
import Decaf.LIRNodes
import Debug.Trace
import qualified Data.Map as M

data VarFact = VarFactMap VarMap
             | VarBot
             deriving (Show)

-- ^ map of variables, extended with a standard bottom element

-- | The fact that goes into the entry of a variable analysis: the first node
-- starts off with no known variables, which is represented by the empty map
-- of labels.
varEntry :: Label -> VarFact
varEntry el = VarFactMap (VarMap M.empty el)

data VarMap = VarMap
    { writeMap :: M.Map LIRReg [Label]
    , current  :: Label
    }
    deriving (Show)
  -- ^ represents varables mapped to the blocks they're found in

writeFactMap :: VarFact -> M.Map LIRReg [Label]
writeFactMap VarBot = M.empty -- lies: an unreachable node appears to have no vars
writeFactMap (VarFactMap (VarMap vm _)) = vm

varLattice:: DataflowLattice VarFact
varLattice = DataflowLattice
  { fact_name = "variables"
  , fact_bot  = VarBot
  , fact_join = joinVar }

joinVar :: JoinFun VarFact
joinVar _ (OldFact VarBot) (NewFact a) = trace ("join called") (SomeChange, a)
joinVar _ (OldFact a) (NewFact VarBot) = trace ("join called") (SomeChange, a)
joinVar _ (OldFact (VarFactMap (VarMap vm _))) (NewFact (VarFactMap (VarMap vm' _))) =
    (changeIf $
      (trace ("difference: " ++ show (vm `M.difference` vm')) (vm `M.difference` vm'))
            /= M.empty
    , VarFactMap $ VarMap (M.union vm vm') (LIRLabel "unset" (-2)))

varTransfer :: FwdTransfer LIRNode VarFact
varTransfer = mkFTransfer unwrapFactFt
  where
    unwrapFactFt :: LIRNode e x -> VarFact -> Fact x VarFact
    unwrapFactFt n VarBot = error "varTransfer:unwrapFactFt called on Bot; should not happen!"
    unwrapFactFt n (VarFactMap (vm@VarMap {})) = ft n vm

    ft :: LIRNode e x -> VarMap -> Fact x VarFact
    ft (LIRLabelNode label) vm      = VarFactMap vm {current = label}
    ft (LIRRegAssignNode x _) vm    = updateFact x vm
    ft (LIRRegOffAssignNode x _ _ _) vm
                                    = updateFact x vm
    ft (LIRStoreNode {}) vm         = VarFactMap vm
    ft (LIRLoadNode x _) vm         = updateFact x vm
    ft (LIRCallNode proc ret) vm    = mkFactBase varLattice [(proc, VarFactMap vm), (ret, VarFactMap vm)]
    ft (LIRCalloutNode {}) vm       = VarFactMap vm
    ft (LIREnterNode {}) vm         = VarFactMap vm
    ft (LIRRetNode successors _) vm = mkFactBase varLattice (map (\x -> (x, VarFactMap vm)) successors)
    ft (LIRIfNode expr tl fl) vm    = mkFactBase varLattice [(tl, VarFactMap vm), (fl, VarFactMap vm)]
    ft (LIRJumpLabelNode l) vm      = mkFactBase varLattice [(l, VarFactMap vm)]

    -- updateFact :: LIRReg -> VarMap -> VarFact
    updateFact k (VarMap vm current) =
        let newList = case M.lookup k vm of
              Just res -> current:res
              Nothing -> [current]
            newMap = M.insert k newList vm 
            newFactMap = VarFactMap (VarMap newMap current)
        in newFactMap -- mkFactBase varLattice [(current, trace ("newFactMap: " ++ show newFactMap) newFactMap)]

-- | var pass
--varPass :: (NonLocal n, Monad m) => FwdPass m n VarFact
varPass = FwdPass
  { fp_lattice = varLattice
  , fp_transfer = varTransfer
  , fp_rewrite = noFwdRewrite
  }
