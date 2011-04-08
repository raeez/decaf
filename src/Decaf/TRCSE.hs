module Decaf.TRCSE where
import Compiler.Hoopl hiding (Top)
import Data.Map as Map
import Decaf.IR.LIR
import Control.Monad
import Decaf.HooplNodes


----------------------------------------------------
-- this file contains CSE transfer/rewrite functions
-- currently it is local rewrite,  
-- would need to expand to global rewrite later



-- Note: 
-- currently only support
-- x = y binop z   -->  x = w 
-- not  x = y relop z, or x = uniop y



data CSEKey = CSEKey LIROperand LIRBinOp LIROperand
type CSEData = LIRReg
type CSERecord = (CSEKey, CSEData)
              deriving (Show, Eq, Typeable)

type CSELattice = [CSERecord]



-- join two CSE lattices : fix me!     (right now, the joined results is always empty, CSE is completely local to basic block)
joinCSELattice :: JoinFun CSELattice
joinCSELattice _ (OldFact m) (NewFact n) = 
  (NoChange, []);
        

  

-- define const lattice
constLattice :: DataflowLattice CSELattice
constLattice = DataflowLattice
  { fact_name   = "CSElattice"
  , fact_bot    = empty        
  , fact_join   = joinCSELattice }



-- remvoe all records containing x, because x has been changed
varChanged :: CSELattice -> LIRReg -> CSELattice
varChanged lat x = filter ok lat
                   where 
                     -- a record is ok (to keep) if it does not contain x
                     ok :: CSERecord -> Bool
                     ok (CSEKey a op b, c) = (oprMismatch a) && (oprMismatch b) && (c /= x)
                     
                     -- a operand does not contains x if it is not a RegOperand made with x
                     oprMismatch :: LIROperand -> Bool
                     oprMismatch (LIRRegOperand x') = (x /= x')  -- if it is a RegOperand, then change x /= x'
                     oprMismatch _                  = True       -- mismatch always holds otherwise
                     




-- lookup expression 
lookupExpr :: CSELattice -> CSEKey -> Maybe LIRReg 
lookupExpr lat k = 
  let recs = filter (\(x,y) -> if x == k then True else False) lat   -- filter out only expression with key match
  in  if recs == [] then Nothing else Just $ snd $ recs !! 0         -- pick the first match





-- define CSE lattice transfer function:  a expression (x op y) is available iff ((x,op,y),w) \in Lattice
-- concerns only RegAssignNode and LoadNode
exprIsAvail :: FwdTransfer Node CSELattice
exprIsAvail = mkFTransfer ft
  where
    ft :: Node e x -> CSELattice -> Fact x CSELattice
    ft (LIRLabelNode _) f = f
    ft (LIRRegAssignNode x (LIRBinExpr a op b)) f = ((a, op, b), x) : (varChanged f x)  -- a op b -> x
    ft (LIRRegAssignNode x _) f = f                                                     -- x = _, no change
    ft (LIRJumpLabelNode l) f = mkFactBase [(l, f)]                    -- jmp l --> associate f with l 
    ft (LIRIfNode expr tl fl) f = mkFactBase [(tl, f), (fl, f)]        -- if expr the jmp tl else jmp fl
    
    ft (LIRTempEnterNode _) f = f
    ft (LIRRegOffAssignNode _ _ _ _) f = f 
    ft (LIRStoreNode _ _) f = f
    ft (LIRLoadNode x _) f = varChanged f x          -- remove all records containing x
    -- may need add more










-- define constant folding rewrites
cse :: Monad m => FwdRewrite m Node f
cse = deepFwdRw simp
  where
    simp :: Node -> Fact x CSELattice -> m (Maybe (Graph n e x))
    simp node _ = return $ liftM nodeToG $ s_node node
  
    s_node :: Node e x -> Maybe (Node e x)
    
    -- x = lit op lit     
    s_node (LIRRegAssignNode x (LIRBinExpr o1@(LIRRefOperand r1) op o2@(LIRRegOperand r2))) = 
      case lookupExpr f $ CSEKey o1 op o2 of
        Just a  -> Just $ LIRRegAssignNode x $ LIROperExpr $ LIRRegOperand a   -- 
        Nothing -> Nothing

    -- others do not need CSE
    s_node _ = Nothing







-- define fwd pass
csePass = FwdPass
  { fp_lattice = CSELattice
  , fp_transfer = exprIsAvail
  , fp_rewrite = cse }

