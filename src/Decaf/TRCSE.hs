{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, NoMonomorphismRestriction #-}

module Decaf.TRCSE where
import Data.Map as Map
import Decaf.IR.LIR
import Control.Monad
import Decaf.HooplNodes
import Loligoptl.Dataflow
import Loligoptl.Graph 
import Loligoptl.Fuel 
import Loligoptl.Label 
import Loligoptl.Combinators
import Data.Maybe


----------------------------------------------------
-- this file contains CSE transfer/rewrite functions
-- currently it is local rewrite,  
-- would need to expand to global rewrite later



-- Note: 
-- currently only support
-- x = y binop z   -->  x = w 
-- not  x = y relop z, or x = uniop y



-- CSE data structure, maps (Opr, Op, Opr) key to Var 
data CSEKey = CSEKey LIROperand LIRBinOp LIROperand
               deriving (Show, Eq)
type CSEData = LIRReg
type CSERecord = (CSEKey, CSEData)


type CSELattice = [CSERecord]




-- join two CSE lattices : fix me!     (right now, the joined results is always empty, CSE is completely local to basic block)
joinCSELattice :: CSELattice -> CSELattice -> (ChangeFlag, CSELattice)
joinCSELattice _ _ = (NoChange, [])
        

  

-- define const lattice
constLattice :: DataflowLattice CSELattice
constLattice = DataflowLattice
  { factBottom = []
  , factJoin   = joinCSELattice }



-- aux: remvoe all records containing x, because x has been changed
varChanged :: CSELattice -> LIRReg -> CSELattice
varChanged lat x = Prelude.filter ok lat
  where 
    -- a record is ok (to keep) if it does not contain x
    ok :: CSERecord -> Bool
    ok (CSEKey a op b, c) = (oprMismatch a) && (oprMismatch b) && (c /= x)
                     
    -- a operand does not contains x if it is not a RegOperand made with x
    oprMismatch :: LIROperand -> Bool
    -- if it is a RegOperand, then change x /= x'
    oprMismatch (LIRRegOperand x') = (x /= x')  
    -- mismatch always holds otherwise
    oprMismatch _                  = True       
                     




-- aux: lookup expression 
lookupExpr :: CSELattice -> CSEKey -> Maybe LIRReg 
lookupExpr lat k = 
  -- filter out only expression with key match
  let recs = Prelude.filter (\(x,y) -> if x == k then True else False) lat   
  -- pick the first match
  in  if recs == [] then Nothing else Just $ snd $ recs !! 0         





-- transfer: define CSE lattice transfer function:  a expression (x op y) is available iff ((x,op,y),w) \in Lattice
-- concerns only RegAssignNode and LoadNode
exprIsAvail :: FwdTransfer Node CSELattice
exprIsAvail = mkFTransfer ft
  where
    ft :: Node e x -> CSELattice -> Fact x CSELattice
    ft (LIRLabelNode _) f = f
    ft (LIRRegAssignNode x (LIRBinExpr a op b)) f = ((CSEKey a op b), x) : (varChanged f x)  -- a op b -> x
    ft (LIRRegAssignNode x _) f = f                                                     -- x = _, no change
    ft (LIRJumpLabelNode l) f = mkFactBase constLattice [(l, f)]                    -- jmp l --> associate f with l 
    ft (LIRIfNode expr tl fl) f = mkFactBase constLattice [(tl, f), (fl, f)]        -- if expr the jmp tl else jmp fl
    
    ft (LIRRegOffAssignNode _ _ _ _) f = f 
    ft (LIRStoreNode _ _) f = f
    ft (LIRLoadNode x _) f = varChanged f x          -- remove all records containing x
    -- may need add more










-- rewrite: define constant folding rewrites
cse :: Monad m => FwdRewrite m Node CSELattice
cse = shallowFwdRw simp
  where
    simp :: forall m e x . (ShapeLifter e x, Monad m) => 
            Node e x -> CSELattice -> m (Maybe (Graph Node e x))
    simp node f = return $ liftM nodeToG $ s_node node
  
      where
        s_node :: Node e x -> Maybe (Node e x)
        -- x = lit op lit     
        s_node (LIRRegAssignNode x (LIRBinExpr o1@(LIRRegOperand r1) 
                                           op o2@(LIRRegOperand r2))) = 
          case lookupExpr f $ CSEKey o1 op o2 of
            Just a  -> Just $ LIRRegAssignNode x $ LIROperExpr $ LIRRegOperand a  
            Nothing -> Nothing

        -- others do not need CSE
        s_node _ = Nothing







-- define fwd pass
csePass = FwdPass
  { fpLattice = constLattice
  , fpTransfer = exprIsAvail
  , fpRewrite = cse }

