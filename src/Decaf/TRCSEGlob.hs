module Decaf.TRCSEGlob where
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



data CSEExpr = CSEExpr LIROperand LIRBinOp LIROperand
type CSEVar = LIRReg
data CSEVarVal = Top | E CSEExpr 
type CSELattice = (Map CSEExpr [CSEVar], Map CSEVar CSEVarVal)   -- keeps a bidirectional map



-- join varval
joinVarVal :: CSEVarVal -> CSEVarVal -> (ChangeFlag, CSEVarVal)
joinVarVal Top _ = (NoChange, Top)
joinVarVal _ Top = (SomeChange, Top)
joinVarVal (E a) (E b) = if a == b then (NoChange, E a)
                                  else (SomeChange, Top)





-- join two CSE lattices : fix me!     (right now, the joined results is always empty, CSE is completely local to basic block)
joinCSELattice :: JoinFun CSELattice
joinCSELattice _ (OldFact m) (NewFact n) = 
  -- if  a Var is mapped to different expr in different expr in each fact, then it is Top
  -- remove the expr->var map 
  -- otherwise, union the expr->var map
  let evm = fst m  -- expr to var map 1
      vem = snd m  -- var to expr map 1
      evn = fst n
      ven = snd n
      vm = difference vem ven
      vn = difference ven vem
      vmn = intersection vem ven     -- this is the variables in map 1 and 2, if x->a op b in m, and x->a' op' b' in n
                                     -- then x->a op b stays if (a op b) == (a' op' b')
                                     -- otherwise x->Top, and (a op b)->x and (a' op' b')->x are removed from evm and evn
      
      -- for each variable in vmn, we need to join their state
      mnjoin = map (\x -> 
                    let vm = Map.lookup x vem
                        vn = Map.lookup x ven
                    in joinVarVal vm vn) $ keys vmn
      vmn' = map (\(f, s) -> s) mnjoin
      cf   = Map.fold (\(f, s) f' -> joinChangeFlag f f') NoChange mnjoin 
      ve   = union vm $ union vn vmn'  -- this is the outgoing var to expr map
      
      -- need to find those variables newly become top, and remove the Expr->Var map
      top  = filter (\(f, s) -> xx f s  
                                where 
                                  xx :: ChangeFlag -> CSEVarVal -> Bool
                                  xx SomeChange Top = True
                                  xx _ _            = False) mnjoin 
      topks = keys top
      topksm = getRelevantKeys vem  -- find all keys in topks, where it maps to a expr in m
      topksn = getRelevantKeys ven  -- ..                                                 n
               where 
                 getRelevantKeys :: Map CSEVar CSEVarVal -> [CSEVar]
                 getRelevantKeys ve = 
                   filter (\x -> case Map.lookup x ve of
                              Top -> False
                              E _ -> True) topks
      
      -- for each x \in topksm, x -> E e is in vem, we need to remove e -> x from evm; the same needs to be done for n
      evm' = removeMap evm vem topksm
      evn' = removeMap evn ven topksn
               where 
                 removeMap :: Map CSEExpr CSEVar -> Map CSEVar CSEVarVal -> [CSEVar] -> Map CSEExpr CSEVar
                 removeMap ev ve ks = 
                   foldr (\x f -> 
                     let E e   = Map.lookup x ve
                         vars  = Map.lookup e ev
                         vars' = delete x vars
                         mp'   = delete e ev       -- delete record
                         mp''  = if vars' == [] then mp' else Map.insert e vars' mp' 
                     in  mp') ev ks
      
      -- now need to union evm' evn'
      ev = unionWith (++) evm' evn'    -- this is the outgoing expr to var map
      
      -- now need to get change flag of ev 
      cf' = cf || (vn /= empty)
  in (cf', (ev, ve))                                    
  --(NoChange, (fromList [], fromList []));
        

  

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

