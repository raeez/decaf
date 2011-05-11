{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, NoMonomorphismRestriction #-}


module Decaf.Passes.Const where
import Data.Map as Map
import Decaf.IR.LIR
import Control.Monad
import Decaf.LIRNodes
import Loligoptl.Dataflow
import Loligoptl.Graph 
import Loligoptl.Fuel 
import Loligoptl.Label 
import Loligoptl.Combinators
import Data.Maybe


----------------------------------
-- Define lattice

data State = Top | I LIRInt
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









-- define constant lattice transfer function
varHasLit :: FwdTransfer Node Lattice
varHasLit = mkFTransfer ft
  where
    ft :: Node e x -> Lattice -> Fact x Lattice
    ft (LIRLabelNode _) f = f
    ft (LIRRegAssignNode x (LIROperExpr (LIRIntOperand k))) f = Map.insert x (I k) f    -- x = 5 --> (x, 5)   
    ft (LIRRegAssignNode x _) f = f                                                              -- x = _, no change
    ft (LIRJumpLabelNode l) f = mkFactBase constLattice [(l, f)]                    -- jmp l --> associate f with l 
    ft (LIRIfNode expr tl fl) f = mkFactBase constLattice [(tl, f), (fl, f)]        -- if expr the jmp tl else jmp fl
  
    ft (LIRRegOffAssignNode _ _ _ _) f = f 
    ft (LIRStoreNode _ _) f = f
    ft (LIRLoadNode x _) f = Map.insert x Top f     -- loaded value are uncertain
    
  -- may need add more
    ft (LIRCalloutNode _) f = f
    ft (LIREnterNode _) f = f    
    --ft LIRRetNode f = f    
    --ft (LIRCallNode _ _) f = f

{-
  LIRCalloutNode      :: String -> Node O O -- assume control falls through
  LIREnterNode        :: LIRInt -> Node O O
  LIRRetNode          :: Node O C
  LIRCallNode         :: LIRLabel -> Maybe LIRLabel -> Node O C -- method label and label for next line
-}

-- xor
xor :: LIRInt -> LIRInt -> LIRInt 
xor x y = if x /= y then 1 else 0

and :: LIRInt -> LIRInt -> LIRInt
and x y = if x == 1 && y == 1 then 1 else 0

or :: LIRInt -> LIRInt -> LIRInt
or x y = if x == 1 || y == 1 then 1 else 0
  
not :: LIRInt -> LIRInt
not x = if x == 0 then 1 else 0






-- define constant folding rewrites
simplify :: Monad m => FwdRewrite m Node Lattice
simplify = shallowFwdRw simp
  where
    simp :: forall e x m f . (Monad m, ShapeLifter e x) => Node e x -> f -> m (Maybe (Graph Node e x))
    simp node _ = return $ liftM nodeToG $ s_node node
  
    s_node :: forall e x . Node e x -> Maybe (Node e x)
    -- x = lit op lit     
    s_node (LIRRegAssignNode x (LIRBinExpr (LIRIntOperand (i1)) op (LIRIntOperand (i2)))) = 
      case op of 
        LADD -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand (i1 + i2))
        LSUB -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand (i1 - i2))
        LMUL -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand (i1 * i2))
        LDIV -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand (i1 `div` i2))
        LMOD -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand (i1 `mod` i2))
        LAND -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand (i1 `Decaf.TRConst.and` i2))
        LOR  -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand (i1 `Decaf.TRConst.or` i2))
        LXOR -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand (i1 `xor` i2))
        -- not understand what these three do
        --LSHL
        --LSHR
        --LSHRA



    -- x = op lit
    s_node (LIRRegAssignNode x (LIRUnExpr op (LIRIntOperand (i)))) = 
      case op of
        LNEG -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand (-i))
        LNOT -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand $ Decaf.TRConst.not i)

    -- if lit op lit then jmp tl else jmp fl 
    s_node (LIRIfNode (LIRBinRelExpr (LIRIntOperand (i1)) op (LIRIntOperand (i2))) tl fl) = 
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
    s_node (LIRIfNode (LIRNotRelExpr (LIRIntOperand (i))) tl fl) = 
        if i == 0 then Just $ LIRJumpLabelNode tl    -- if not i
                  else Just $ LIRJumpLabelNode fl

    -- if lit then jmp tl else jmp fl
    s_node (LIRIfNode (LIROperRelExpr (LIRIntOperand (i))) tl fl) = 
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
        map_node _ = Nothing

        -- prop constant in a LIROperand 
        propOperand :: LIROperand -> LIROperand
        propOperand (LIRRegOperand x) = 
          case Map.lookup x f of 
            Nothing    -> LIRRegOperand x
            Just Top   -> LIRRegOperand x
            Just (I i) -> LIRIntOperand i
        propOperand opr = opr   





-- define fwd pass
constPropPass = FwdPass
  { fpLattice = constLattice
  , fpTransfer = varHasLit
  , fpRewrite = constProp }    -- `thenFwdRw` simplify }




  
  
