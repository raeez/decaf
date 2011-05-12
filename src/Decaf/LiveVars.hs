{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, NoMonomorphismRestriction #-}

module Decaf.LiveVars 
  ( csePass
  , VarFact
  )

where
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
import Debug.Trace

import qualified Data.Map as M

type Key = LIRReg

type VarFact = S.Set Key 

-- join two CSE facts 
join :: VarFact -> VarFact -> (ChangeFlag, VarFact)
join = S.union




-- transfer: define CSE lattice transfer function:  a expression (x op y) is available iff ((x,op,y),w) \in Lattice
-- concerns only RegAssignNode and LoadNode
transfer :: FwdTransfer Node CSEFact
transfer = mkFTransfer livetran
  where
    livetran :: Node e x -> VarFact -> Fact x VarFact

    livetran (LIRRegAssignNode x y) f = 
      case y of 
        (LIRBinExpr op1 _ op2) -> opinsert op1 (opinsert op2 f')
        (LIRUnExpr _ op) -> opinsert op f'
        (LIROperExpr op) -> opinsert op f'
        
      where f' = vardelete x f

    livetran (LIRRegOffAssignNode r1 r2 _ op) = 
      opinsert  op $
      reginsert r1 $
      reginsert r2 $ 
    livetran (LIRStoreNode m op) = 
      opinsert op $
      meminsert m $ 
    livetran (LIRLoadNode r m) = 
      reginsert r $
      meminsert m $

    livetran (LIRIfNode expr fl tl) f = 
      mkFactBase lattice [(fl, f'), (tl, f')]
      where
        f' = case expr of 
               (LIRBinRelExpr op1 _ op2) -> opinsert op1 $ opinsert op2 f
               (LIRNotRelExpr op) -> opinsert op f
               (LIROperRelExpr op) -> opinsert op f

    livetran (LIRJumpLabelNode l) f = = mkFactBase lattice [(l,f)]
    -- these two should change
    livetran (LIRCallNode procl retl) f = mkFactBase lattice [(procl, f), (retl, f)]
    livetran (LIRRetNode successors _) f = mkFactBase lattice []

    livetran (LIRLabelNode {}) f         = CSEFactMap f
    livetran (LIRCalloutNode {}) f       = CSEFactMap f
    livetran (LIREnterNode {}) f         = CSEFactMap f

    vardelete = S.delete
    reginsert = S.insert
    -- only inserts registers
    opinsert :: LIROperand -> VarFact -> VarFact
    opinsert (LIRRegOperand r) = reginsert r
    opinsert _ = id

    meminsert :: LIRMemAddr -> VarFact -> VarFact
    meminsert (LIRMemAddr r mr _ _) = 
      reginsert r $
      case mr of 
        Just r' -> reginsert r'
        Nothing -> id


-- rewrite: define constant folding rewrites
rewrite :: Monad m => FwdRewrite m Node CSEFact
rewrite = shallowFwdRw (return Nothing)
{-  where
    rew :: forall m e x . (ShapeLifter e x, Monad m) => 
            Node e x -> CSEFact -> m (Maybe (Graph Node e x))
    rew _ _ = return Nothing
-}
-- define lattice
bottom = S.empty
lattice = DataflowLattice
  { factBottom = bottom
  , factJoin   = join }


-- define fwd pass
liveVarPass  = FwdPass 
  { fpLattice = lattice
  , fpTransfer = transfer
  , fpRewrite = rewrite }
