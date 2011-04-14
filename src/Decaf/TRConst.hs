{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, NoMonomorphismRestriction #-}


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
mkFTransfer :: (forall e x. n e x -> f -> Fact x f) -> FwdTransfer n f
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
deepFwdRw :: forall m e x n f. (Monad m) => (forall e x. n e x -> f -> m (Maybe (Graph n e x))) -> FwdRewrite m n f
deepFwdRw rw = FwdRewrite3 { getFwdRewrite3 = (rw', rw', rw') }
              where 
                rw' :: forall e x. n e x -> f -> m (Maybe (FwdRev m n f e x))
                rw' n f = do 
                  x <- rw n f 
                  case x of 
                    Nothing -> return Nothing
                    Just x' -> return $ Just $ FwdRev x' $ deepFwdRw rw
                    

shallowFwdRw :: forall m e x n f . (Monad m) => (forall e x . (ShapeLifter e x) => n e x -> f -> m (Maybe (Graph n e x))) -> FwdRewrite m n f
shallowFwdRw rw = FwdRewrite3 { getFwdRewrite3 = (rw', rw', rw') }
                  where
                    rw' :: forall e x . (ShapeLifter e x) => n e x -> f -> m (Maybe (FwdRev m n f e x))
                    rw' n f = do
                      x <- rw n f
                      case x of
                        Nothing -> return Nothing
                        Just x' -> return $ Just $ FwdRev x' emptyRw
                    
                    emptyRw :: forall m n f . (Monad m) => FwdRewrite m n f
                    emptyRw = FwdRewrite3 { getFwdRewrite3 = (rw'', rw'', rw'') }
                    
                    rw'' :: forall m n f e x . (Monad m) => n e x -> f -> m (Maybe (FwdRev m n f e x))
                    rw'' _ _ = return Nothing




-- define constant folding rewrites
simplify :: Monad m => FwdRewrite m Node Lattice
simplify = shallowFwdRw simp
  where
    simp :: forall e x m f . (Monad m, ShapeLifter e x) => Node e x -> f -> m (Maybe (Graph Node e x))
    simp node _ = return $ liftM nodeToG $ s_node node
  
    s_node :: forall e x . Node e x -> Maybe (Node e x)
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
    s_node (LIRIfNode (LIRBinRelExpr (LIRIntOperand (LIRInt i1)) op (LIRIntOperand (LIRInt i2))) tl fl) = 
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
constProp = shallowFwdRw cp   -- shallowFwdRw is not defined
  where
    cp :: forall e x m f. (ShapeLifter e x, Monad m) => Node e x -> Lattice -> m (Maybe (Graph Node e x))    
    cp node f  = return $ liftM nodeToG $ map_node node
      where 
        map_node :: forall e x . (ShapeLifter e x) => Node e x -> Maybe (Node e x)
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
                  Just $ LIRStoreNode a $ propOperand opr

        -- operand in if relexpr
        map_node (LIRIfNode (LIRBinRelExpr opr1 op opr2) tl fl) =
                  Just $ LIRIfNode (LIRBinRelExpr (propOperand opr1) op (propOperand opr2)) tl fl
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
            Nothing    -> LIRRegOperand x
            Just Top   -> LIRRegOperand x
            Just (I i) -> LIRIntOperand $ LIRInt i
        propOperand opr = opr   








-- combines two FwdRewrte3
{-
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
    FwdRewrite3 (tuple2rev' $ wrap2 (rev2tuple' f1) (rev2tuple' f2), 
                 tuple2rev' $ wrap2 (rev2tuple' m1) (rev2tuple' m2), 
                 tuple2rev' $ wrap2 (rev2tuple' l1) (rev2tuple' l2))
  where
    rev2tuple' :: forall m n f e x. (n e x -> f -> m (Maybe (FwdRev m n f e x))) -> 
                                    (n e x -> f -> m (Maybe (Graph n e x, FwdRewrite m n f)))
    rev2tuple' f = (fmap rev2tuple) . f
    rev2tuple :: forall m n f e x. FwdRev m n f e x -> (Graph n e x, FwdRewrite m n f)
    rev2tuple (FwdRev g rw) = (g, rw)
                            
    tuple2rev' :: forall m n f e x . (n e x -> f -> m (Maybe (Graph n e x, FwdRewrite m n f))) ->
                                     (n e x -> f -> m (Maybe (FwdRev m n f e x))) 
    tuple2rev' f = (fmap tuple2rev) . f                                     
    tuple2rev :: forall m n f e x. (Graph n e x, FwdRewrite m n f) -> FwdRev m n f e x
    tuple2rev (g, rw) = FwdRev g rw
                                     






-- combines two FwdRewrite
thenFwdRw :: forall m n f. Monad m
          => FwdRewrite m n f
          -> FwdRewrite m n f
          -> FwdRewrite m n f                       -- proposition
thenFwdRw rw3 rw3' = wrapFR2 thenrw rw3 rw3'        -- proof 
 where
  thenrw :: forall m1 e x t t1 m n f.
            Monad m1 =>
            (t -> t1 -> m1 (Maybe (Graph n e x, FwdRewrite m n f))) ->
            (t -> t1 -> m1 (Maybe (Graph n e x, FwdRewrite m n f))) ->
            t -> t1 -> m1 (Maybe (Graph n e x, FwdRewrite m n f))
  thenrw rw rw' n f = rw n f >>= fwdRes
     where fwdRes Nothing   = rw' n f     -- if rw does not rewrite anything, then apply rw'
           fwdRes (Just gr) = return $ Just $ fadd_rw rw3' gr    -- otherwise, apply rw', then the rw returned by rw n f 


-- | Function inspired by 'add' in the paper
fadd_rw :: Monad m
       => FwdRewrite m n f
       -> (Graph n e x, FwdRewrite m n f)
       -> (Graph n e x, FwdRewrite m n f)
fadd_rw rw2 (g, rw1) = (g, rw1 `thenFwdRw` rw2)
-}





    
    
                                     


-- combines two FwdRewrite
thenFwdRw :: forall m n f e x. Monad m
          => FwdRewrite m n f
          -> FwdRewrite m n f
          -> FwdRewrite m n f                       -- proposition
thenFwdRw rw3 rw3' = wrapFR2 rw3 rw3'        -- proof 
  where
  -- wrap two FwdRewrite together
    wrapFR2 ::  FwdRewrite m n f
             -> FwdRewrite m n f
             -> FwdRewrite m n f      -- see Note [Respects Fuel] 
    wrapFR2 (FwdRewrite3 (f1, m1, l1)) (FwdRewrite3 (f2, m2, l2)) =
        FwdRewrite3 (thenrw f1 f2, thenrw m1 m2, thenrw l1 l2)

    thenrw :: forall e x.
            --Monad m =>
            (n e x -> f -> m (Maybe (FwdRev m n f e x))) ->
            (n e x -> f -> m (Maybe (FwdRev m n f e x))) ->
            n e x -> f -> m (Maybe (FwdRev m n f e x))   
    thenrw rw rw' n f = rw n f >>= fwdRes
     where 
       fwdRes :: --forall m n f e x . 
                 -- Monad m => 
                 --forall e x.
                 (Maybe (FwdRev m n f e x)) -> m (Maybe (FwdRev m n f e x)) 
       -- if rw does not rewrite anything, then apply rw'
       fwdRes Nothing  = rw' n f     
       -- otherwise, apply rw', then the rw returned by rw n f 
       fwdRes (Just gr) = return $ Just $ fadd_rw rw3' gr    

    


-- | Function inspired by 'add' in the paper
-- combine a FwdRewrite with a FwdRev
fadd_rw :: forall m n f e x . Monad m
       => FwdRewrite m n f
       -> FwdRev m n f e x 
       -> FwdRev m n f e x
fadd_rw rw2 (FwdRev g rw1) = FwdRev g (rw1 `thenFwdRw` rw2)







-- define fwd pass
constPropPass = FwdPass
  { fpLattice = constLattice
  , fpTransfer = varHasLit
  , fpRewrite = constProp }    -- `thenFwdRw` simplify }




  
  