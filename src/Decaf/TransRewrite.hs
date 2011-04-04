module Decaf.TransRewrite where
import Compiler.Hoopl hiding (Top)
import Data.Map as Map
import Decaf.IR.LIR



----------------------------------
-- Define lattice

data State = Top | I Int
type Var = LIRReg
type Lattice = Map Var State


-- joint two states
joinState :: State -> State -> (ChangeFlag, State)
joinState Top _ = (NoChange, Top)
joinState _ Top = (SomeChange, Top)
joinState (I i) (I j) = if i == j then (NoChange, I i)
                                  else (SomeChange, Top)
                                                         


-- join two change flag
joinFlag :: ChangeFlag -> ChangeFlag -> ChangeFlag 
joinFlag NoChange NoChange = NoChange
joinFlag _ _               = SomeChange                             




-- join two lattices : perform joinState on each, union two maps,  m1 \ m2 U m2 \ m1 U $ map joinState (m1 ^ m2)
joinLattice :: JoinFun Lattice
joinLattice _ (OldFact m) (NewFact n) = 
  let m' = difference m n
      n' = difference n m
      mn = intersection m n
      mnks = keys mn
      --for any key, lookup its value in m and n, then use joinState on them, return the results in a map       
      mnjoin = Map.map (\x -> 
                     let vm = Map.lookup x m
                         vn = Map.lookup x n
                     in (x, joinState vm vn)) mnks 
      -- remove ChangeFlag in value
      mn' = Map.map (\(f, s) -> s) mnjoin
      -- join all the flags
      cf  = Map.fold (\(f, s) f' -> joinFlag f f') NoChange mnjoin 
   in (cf, union m' $ union n' mn')
      

  

-- define const lattice
constLattice :: DataflowLattice Lattice
constLattice = DataflowLattice
  { fact_name   = "constlattice"
  , fact_bot    = empty        
  , fact_join   = joinLattice }




-- Labels 
data HooplLabel = Hoopl.Label 



-- nodes
data Node e x where
  LIRRegAssignNode :: LIRReg -> LIRExpr -> Node O O
  LIRIfNode        :: LIRRelExpr -> HooplLabel -> HooplLabel -> Node O C 
  LIRJumpLabelNode :: HooplLabel -> Node O C
  LIRLabelNode     :: HooplLabel -> Node C O
  -- add more



-- define constant lattice transfer function
varHasLit :: FwdTransfer Node Lattice
varHasLit = mkFTransfer ft
  where
    ft :: Node e x -> Lattice -> Fact x Lattice
    ft (LIRLabelNode _) f = f
    ft (LIRRegAssignNode x (LIROperExpr (LIRIntOperand (LIRInt k)))) f = Map.insert x (I k) f      -- x = 5 --> (x, 5)   (?) 
    ft (LIRRegAssignNode x _) f = f                                                              -- x = _, no change
    ft (LIRJumpLabelNode l) f = mkFactBase [(l, f)]
    ft (LIRIfNode expr tl fl) f = mkFactBase [(tl, f), (fl, f)] 
    -- add more


-- xor
xor :: Int -> Int -> Int 
xor x y = if x /= y then 1 else 0

and :: Int -> Int -> Int
and x y = if x == 1 && y == 1 then 1 else 0

or :: Int -> Int -> Int
or x y = if x == 1 || y == 1 then 1 else 0
  
not :: Int -> Int
not x = if x == 0 then 1 else 0



-- define constant folding rewrites
simplify :: Monad m => FwdRewrite m Node f
simplify = deepFwdRw simp
  where
    simp :: Node -> Fact x Lattice -> m (Maybe (Graph n e x))
    simp node _ = return $ liftM nodeToG $ s_node node
  
    s_node :: Node e x -> Maybe (Node e x)
    -- x = lit op lit 
    s_node (LIRRefAssignNode x (LIRBinExpr (LIRIntOperand $ LIRInt i1) op (LIRIntOperand $ LIRInt i2))) = 
      case op of 
        LADD -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand $ LIRInt (i1 + i2)
        LSUB -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand $ LIRInt (i1 - i2)
        LMUL -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand $ LIRInt (i1 * i2)
        LDIV -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand $ LIRInt (i1 `div` i2)
        LMOD -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand $ LIRInt (i1 `mod` i2)
        LAND -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand $ LIRInt (i1 `and` i2)
        LOR  -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand $ LIRInt (i1 `or` i2)
        LXOR -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand $ LIRInt (i1 `xor` i2)
        -- add more
    -- x = op lit
    s_node (LIRRefAssignNode x (LIRUnExpr op $ LIRInt i) = 
      case op of
        LNEG -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand $ LIRInt (-i)
        LNOT -> Just $ LIRRegAssignNode x (LIROperExpr $ LIRIntOperand $ LIRInt $ not i

    -- if lit op lit then jmp tl else jmp fl 
    s_node (LIRIfNode (LIRBinRelExpr (LIRIntOperand $ LIRInt i1) op (LIRIntOperand $ LIRInt i2)) tl fl) = 
        if eval i1 op i2 then Just $ LIRJumpLabelNode tl
                       else Just $ LIRJumpLabelNode fl
        where eval i1 op i2 = case op of 
          LEQ  -> i1 == i2
          LNEQ -> i1 /= i2
          LGT  -> i1 >  i2
          LGTE -> i1 >= i2
          LLT  -> i1 <  i2
          LLTE -> i1 <= i2

    -- if not lit then jmp tl else jmp fl
    s_node (LIRIfNode (LIRNotRelExpr $LIRIntOperand $ LIRInt i) tl fl) = 
        if i == 0 then Just $ LIRJumpLabelNode tl    -- if not i
                  else Just $ LIRJumpLabelNode fl

    -- if lit then jmp tl else jmp fl
    s_node (LIRIfNode (LIROperRelExpr $LIRIntOperand $ LIRInt i) tl fl) = 
        if i == 1 then Just $ LIRJumpLabelNode tl
                  else Just $ LIRJumpLabelNode fl

    -- others do not need constant folding
    s_node _ = Nothing








-- define constant prop rewrites, replace operand x with lit if Map.lookup x f == lit
constProp :: Monad m => FwdRewrite m Node Lattice
constProp = shallowFwdRw cp
  where
    cp :: Node -> Fact x Lattice -> m (Maybe (Graph n e x))    
    cp node f  = return $ liftM nodeToG $ map_node node
    where 
      map_node :: Node -> Node
      -- operand in assignment expr
      map_node (LIRRegAssignNode x (LIRBinExpr opr1 op opr2)) = 
                LIRRegAssignNode x (LIRBinExpr (propOperand opr1) op (propOperand opr2))  -- always rewrite
      map_node (LIRRegAssignNode x (LIRUnExpr op opr)) = 
                LIRRegAssignNode x (LIRUnExpr op $ propOperand opr)                       -- always rewrite
      map_node (LIRRegAssignNode x (LIROperExpr opr)) = 
                LIRRegAssignNode x (LIROperExpr $ propOperand opr)                        -- always rewrite
        
      -- operand in if relexpr
      map_node (LIRIfNode (LIRBinRelExpr opr1 op opr2) tl fl) =
                LIRIfNode (LIRBinRelExpr (propOperand opr1) op (propOperand opr2)) tl fl)
      map_node (LIRIfNode (LIRNotRelExpr opr) tl fl) =
                LIRIfNode (LIRNotRelExpr $ propOperand opr) tl fl) 
      map_node (LIRIfNode (LIROperRelExpr opr) tl fl) =
                LIRIfNode (LIROperRelExpr $ propOperand opr) tl fl) 

      -- others do not need const prop
      map_node n = n

      where 
        propOperand (LIRRegOperand x) = 
          case Map.lookup x f of 
            Top   -> LIRRegOperand x
            I i   -> LIRIntOperand $ LIRInt i
        propOperand opr = opr   







-- define fwd pass
constPropPass = FwdPass
  { fp_lattice = constLattice
  , fp_transfer = varHasLit
  , fp_rewrite = constProp ‘thenFwdRw‘ simplify }





{-
type Var = String
type Label = String
data Expr = Var Var | Lit Int
data Lit = L Int

data Node e x where
  LabelSSA :: Label -> Node C O
  AssignSSA :: Var -> Expr -> Node O O
  BranchSSA :: Label -> Node O C
  

type ConstFact = Map Var (WithTop Lit)
constLattice:: DataflowLattice ConstFact
constLattice = DataflowLattice 
 { fact_bot = mapEmpty,
   fact_join = union (extendJoinDomain constFactAdd) }
 where 
   constFactAdd _ (OldFact old) (NewFact new)
     = if new == old then (NoChange, PElem new)
                     else (SomeChange, Top)



--------------------------------------------------
-- Analysis: variable equals a literal constant
varHasLit :: FwdTransfer Node ConstFact
varHasLit = mkFTransfer ft
   where
     ft :: Node e x -> ConstFact -> Fact x ConstFact
     ft (LabelSSA _) f = f
     ft (AssignSSA x _) f = insert x Top f
     ft (BranchSSA l) f = mkFactBase [(l, f)]
-}


  
  