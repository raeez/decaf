{-# LANGUAGE GADTs, RankNTypes #-}

module Decaf.TRConst where
--import Compiler.Hoopl hiding (Top)
import Data.Map as Map
import Decaf.IR.LIR
import Control.Monad
import Decaf.HooplNodes
import Loligoptl.Dataflow
import Loligoptl.Graph 
import Loligoptl.Fuel 
import Loligoptl.Label 
import Data.Maybe


----------------------------------
-- Define lattice

data State = Top | I Int
                   deriving (Eq, Ord, Show)
type Var = LIRReg
type Lattice = Map Var State


-- joint two states
joinState :: State -> State -> (ChangeFlag, State)
joinState Top _ = (NoChange, Top)
joinState _ Top = (SomeChange, Top)
joinState (I i) (I j) = if i == j then (NoChange, I i)
                                  else (SomeChange, Top)
                                                         





-- join two lattices : perform joinState on each, union two maps,  m1 \ m2 U m2 \ m1 U $ map joinState (m1 ^ m2)
joinLattice :: Lattice -> Lattice -> (ChangeFlag, Lattice)
joinLattice m n = 
  let m' = difference m n
      n' = difference n m
      mn = intersection m n
      mnks = keys mn
      --for any key, lookup its value in m and n, then use joinState on them, return the results in a map       
      mnjoin = Prelude.map (\x -> 
                     let vm = fromJust $ Map.lookup x m
                         vn = fromJust $ Map.lookup x n
                     in (x, joinState vm vn)) mnks 
      -- remove ChangeFlag in value
      mn' = fromList $ Prelude.map (\(k, (f, s)) -> (k, s)) mnjoin
      -- join all the flags
      cf  = foldr (\(k, (f, s)) f' -> joinChangeFlag f f') NoChange mnjoin 
      cf' = if n' /= empty then SomeChange else cf   -- logical combine
   in (cf', union m' $ union n' mn')
      

  

-- define const lattice
constLattice :: DataflowLattice Lattice
constLattice = DataflowLattice
  { factBottom  = empty        
  , factJoin    = joinLattice }






-- generate transfer function
mkFTransfer :: (Node e x -> Lattice -> Fact x Lattice) -> FwdTransfer Node Lattice
mkFTransfer t = FwdTransfer3 { getFwdTransfer3 = (t, t, t) }






-- define constant lattice transfer function
varHasLit :: FwdTransfer Node Lattice
varHasLit = mkFTransfer ft
  where
    ft :: Node e x -> Lattice -> Fact x Lattice
    ft (LIRLabelNode _) f = f
    ft (LIRRegAssignNode x (LIROperExpr (LIRIntOperand (LIRInt k)))) f = Map.insert x (I k) f    -- x = 5 --> (x, 5)   
    ft (LIRRegAssignNode x _) f = f                                                              -- x = _, no change
    ft (LIRJumpLabelNode l) f = mkFactBase constLattice [(l, f)]                    -- jmp l --> associate f with l 
    ft (LIRIfNode expr tl fl) f = mkFactBase constLattice [(tl, f), (fl, f)]        -- if expr the jmp tl else jmp fl
    
    ft (LIRTempEnterNode _) f = f
    ft (LIRRegOffAssignNode _ _ _ _) f = f 
    ft (LIRStoreNode _ _) f = f
    ft (LIRLoadNode x _) f = Map.insert x Top f     -- loaded value are uncertain
    -- may need add more


-- xor
xor :: Int -> Int -> Int 
xor x y = if x /= y then 1 else 0

and :: Int -> Int -> Int
and x y = if x == 1 && y == 1 then 1 else 0

or :: Int -> Int -> Int
or x y = if x == 1 || y == 1 then 1 else 0
  
not :: Int -> Int
not x = if x == 0 then 1 else 0





-- create deep rewrite function
deepFwdRw :: Monad m => (Node e x -> Fact x Lattice -> m (Maybe (Graph n e x))) -> FwdRewrite m Node f
deepFwdRw rw = FwdRewrite3 { getFwdRewrite3 = (rw', rw', rw') }
              where 
                rw' :: Monad m => Node e x -> Fact x Lattice -> m (Maybe (FwdRev m n f e x))
                rw' n f = do 
                  x <- rw n f 
                  case x of 
                    Nothing -> return Nothing
                    Just x' -> return $ Just $ FwdRev x' $ deepFwdRw rw
                    



-- define constant folding rewrites
simplify :: Monad m => FwdRewrite m Node f
simplify = deepFwdRw simp
  where
    simp :: (Monad m, ShapeLifter e x) => Node e x -> Fact x Lattice -> m (Maybe (Graph Node e x))
    simp node _ = return $ liftM nodeToG $ s_node node
  
    s_node :: Node e x -> Maybe (Node e x)
    -- x = lit op lit     
    s_node (LIRRegAssignNode x (LIRBinExpr (LIRIntOperand (LIRInt i1)) op (LIRIntOperand (LIRInt i2)))) = 
      case op of 
        LADD -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand $ LIRInt (i1 + i2))
        LSUB -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand $ LIRInt (i1 - i2))
        LMUL -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand $ LIRInt (i1 * i2))
        LDIV -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand $ LIRInt (i1 `div` i2))
        LMOD -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand $ LIRInt (i1 `mod` i2))
        LAND -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand $ LIRInt (i1 `Decaf.TRConst.and` i2))
        LOR  -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand $ LIRInt (i1 `Decaf.TRConst.or` i2))
        LXOR -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand $ LIRInt (i1 `xor` i2))
        -- not understand what these three do
        --LSHL
        --LSHR
        --LSHRA

    -- x = op lit
    s_node (LIRRegAssignNode x (LIRUnExpr op (LIRIntOperand (LIRInt i)))) = 
      case op of
        LNEG -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand $ LIRInt (-i))
        LNOT -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand $ LIRInt $ Decaf.TRConst.not i)

    -- if lit op lit then jmp tl else jmp fl 
    s_node (LIRIfNode (LIRBinRelExpr (LIRIntOperand (LIRInt i1)) op (LIRIntOperand $ LIRInt i2)) tl fl) = 
        if eval i1 op i2 then Just $ LIRJumpLabelNode tl
                       else Just $ LIRJumpLabelNode fl
        where 
          eval i1 op i2 = 
            case op of 
              LEQ  -> (i1 == i2)
              LNEQ -> (i1 /= i2)
              LGT  -> i1 >  i2
              LGTE -> i1 >= i2
              LLT  -> i1 <  i2
              LLTE -> i1 <= i2

    -- if not lit then jmp tl else jmp fl
    s_node (LIRIfNode (LIRNotRelExpr (LIRIntOperand (LIRInt i))) tl fl) = 
        if i == 0 then Just $ LIRJumpLabelNode tl    -- if not i
                  else Just $ LIRJumpLabelNode fl

    -- if lit then jmp tl else jmp fl
    s_node (LIRIfNode (LIROperRelExpr (LIRIntOperand (LIRInt i))) tl fl) = 
        if i == 1 then Just $ LIRJumpLabelNode tl
                  else Just $ LIRJumpLabelNode fl

    -- others do not need constant folding
    s_node _ = Nothing








-- define constant prop rewrites, replace operand x with lit if Map.lookup x f == lit
constProp :: Monad m => FwdRewrite m Node Lattice
constProp = deepFwdRw cp   -- shallowFwdRw is not defined
  where
    cp :: Node e x -> Fact x Lattice -> m (Maybe (Graph n e x))    
    cp node f  = return $ liftM nodeToG $ map_node node
      where 
        map_node :: Node e x -> Maybe (Node e x)
        -- operand in assignment expr
        map_node (LIRRegAssignNode x (LIRBinExpr opr1 op opr2)) = 
                  Just $ LIRRegAssignNode x (LIRBinExpr (propOperand opr1) op (propOperand opr2))  -- always rewrite
        map_node (LIRRegAssignNode x (LIRUnExpr op opr)) = 
                  Just $ LIRRegAssignNode x (LIRUnExpr op $ propOperand opr)                       -- always rewrite
        map_node (LIRRegAssignNode x (LIROperExpr opr)) = 
                  Just $ LIRRegAssignNode x (LIROperExpr $ propOperand opr)                        -- always rewrite
      
        -- operand in offset assignment 
        map_node (LIRRegOffAssignNode x y s opr) = 
                  Just $ LIRRegOffAssignNode x y s $ propOperand opr

        -- operand in store 
        map_node (LIRStoreNode a opr) = 
                  Just $ LIRStoreNode a $ propOperand

        -- operand in if relexpr
        map_node (LIRIfNode (LIRBinRelExpr opr1 op opr2) tl fl) =
                  Just $ LIRIfNode (LIRBinRelExpr (propOperand opr1) op (propOperand opr2) tl fl)
        map_node (LIRIfNode (LIRNotRelExpr opr) tl fl) =
                  Just $ LIRIfNode (LIRNotRelExpr $ propOperand opr) tl fl 
        map_node (LIRIfNode (LIROperRelExpr opr) tl fl) =
                  Just $ LIRIfNode (LIROperRelExpr $ propOperand opr) tl fl

        -- others do not need const prop
        map_node n = Just n

        -- prop constant in a LIROperand 
        propOperand :: LIROperand -> LIROperand
        propOperand (LIRRegOperand x) = 
          case Map.lookup x f of 
            Top   -> LIRRegOperand x
            I i   -> LIRIntOperand $ LIRInt i
        propOperand opr = opr   








-- combines two FwdRewrte3
wrapFR2
  :: (forall e x . (n1 e x -> f1 -> m1 (Maybe (Graph n1 e x, FwdRewrite m1 n1 f1))) ->
                   (n2 e x -> f2 -> m2 (Maybe (Graph n2 e x, FwdRewrite m2 n2 f2))) ->
                   (n3 e x -> f3 -> m3 (Maybe (Graph n3 e x, FwdRewrite m3 n3 f3)))
  )
                 -- ^ This argument may assume that any function passed to it      
                 -- respects fuel, and it must return a result that respects fuel.        
     -> FwdRewrite m1 n1 f1
     -> FwdRewrite m2 n2 f2
     -> FwdRewrite m3 n3 f3      -- see Note [Respects Fuel]                                
wrapFR2 wrap2 (FwdRewrite3 (f1, m1, l1)) (FwdRewrite3 (f2, m2, l2)) =
  FwdRewrite3 (wrap2 f1 f2, wrap2 m1 m2, wrap2 l1 l2)



-- combines two FwdRewrite
thenFwdRw :: forall m n f. Monad m
          => FwdRewrite m n f
          -> FwdRewrite m n f
          -> FwdRewrite m n f
thenFwdRw rw3 rw3' = wrapFR2 thenrw rw3 rw3'
 where
  thenrw :: forall m1 e x t t1 m n f.
            Monad m1 =>
            (t -> t1 -> m1 (Maybe (Graph n e x, FwdRewrite m n f)))
            -> (t -> t1 -> m1 (Maybe (Graph n e x, FwdRewrite m n f)))
            -> t
            -> t1
            -> m1 (Maybe (Graph n e x, FwdRewrite m n f))
  thenrw rw rw' n f = rw n f >>= fwdRes
     where fwdRes Nothing   = rw' n f
           fwdRes (Just gr) = return $ Just $ fadd_rw rw3' gr




-- | Function inspired by 'add' in the paper                                                                        
fadd_rw :: Monad m
           => FwdRewrite m n f
           -> (Graph n e x, FwdRewrite m n f)
           -> (Graph n e x, FwdRewrite m n f)
fadd_rw rw2 (g, rw1) = (g, rw1 `thenFwdRw` rw2)





-- define fwd pass
constPropPass = FwdPass
  { fpLattice = constLattice
  , fpTransfer = varHasLit
  , fpRewrite = constProp `thenFwdRw` simplify }




  
  