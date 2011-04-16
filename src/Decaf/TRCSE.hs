{-
{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, NoMonomorphismRestriction #-}

module Decaf.TRCSE 
  ( csePass
  , CSEFact
  )
where
import Decaf.Util.Prelude
import Decaf.IR.LIR
import Decaf.HooplNodes

import Loligoptl.Dataflow
import Loligoptl.Graph 
import Loligoptl.Fuel 
import Loligoptl.Label 
import Loligoptl.Combinators

import Control.Monad
import Data.Maybe
import Data.Int
import qualified Data.Map as M

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


type CSEValue = Int
type CSETemp  = Int
data MaybeV = JustV CSEValue 
            | LiteralV Int64
            | VBottom
            | VTop
              deriving Eq
data MaybeT = JustT CSETemp
            | TBottom
            | TTop
              deriving Eq

toMaybeV :: LIROperand -> (M.Map LIRReg MaybeV) -> Maybe MaybeV
toMaybeV (LIRRegOperand r) m = JustV $ M.lookup r m
toMaybeV (LIRIntOperand i) m = LiteralV i


type CSEMaps = 
  ( M.Map LIRReg MaybeV
  , M.Map (MaybeV, LIRBinOp, MaybeV) MaybeV
  , M.Map (MaybeV, LIRBinOp, MaybeV) MaybeT)

-- label count, 
type CSEFact = StateMonad (Int, CSEMaps)
getMaps    = SM(\s -> (snd s, s))
setMaps m' = SM(\(c,m) -> ((),(c,m')))
getCount   = SM(\(c,m) -> (c, (c+1,m)))

mkState i = (i, bottom)

{-
type CSERecord = (CSEKey, CSEData)
type CSEFact = [CSERecord]

-- join two CSE lattices : fix me!     (right now, the joined results is always empty, CSE is completely local to basic block)
joinCSEFact :: CSEFact -> CSEFact -> (ChangeFlag, CSEFact)
joinCSEFact [] (x:xs) = (SomeChange, (x:xs))
--joinCSEFact x (y:ys) = (SomeChange, (x:xs))
joinCSEFact x _ = (NoChange, x)-}


-- define const lattice
cseLattice :: DataflowLattice (CSEFact a)
cseLattice = DataflowLattice
  { factBottom = (M.empty,M.empty,M.empty)
  , factJoin   = joinCSEFact }

bottom = factBottom cseLattice

joinCSEFact :: CSEFact -> CSEFact -> (ChangeFlag, CSEFact)
joinCSEFact a b 
  | a == bottom && b /= bottom =
    (SomeChange, b)
  | a == b = 
    (NoChange a)
  | a /= bottom && b /= bottom = 
    join a b
  | otherwise =  -- otherwise, b is bottom
    (NoChange, a)
  where 
    join :: CSEFact -> CSEFact -> (ChangeFlag, CSEFact) -- nontrivial, nonequivalent facts
    join m1 m2 = 
      let (join' (rv1,vv1,vt1) (rv2,vv2,vt2) = 
      let m1 = M.unionWith joinRV rv1 rv2
          m2 = M.unionWith joinVV vv1 vv2
          m3 = M.unionWith joinVT vt1 vt2
      in
        ( or (map sc (M.toList m1 ++ M.toList m2 ++ M.toList m3))
        , (M.map snd m1, M.map snd m2, M.map snd m3))
      where
        sc keyvals = or (map ((== SomeChange) . fst) keyvals)

    joinRV :: MaybeV -> MaybeV -> MaybeV
    joinRV VBottom x = (SomeChange, x)
    joinRV x VBottom = (NoChange, x)
    joinRV VTop x = (NoChange, VTop)
    joinRV x VTop = (SomeChange, VTop)
    joinRV x y | x == y = (NoChange, x)
    joinRV x y = (SomeChange, VTop)

    joinVV = joinRV

    joinVT :: MaybeT -> MaybeT -> MaybeT
    joinVT TBottom x = (SomeChange, x)
    joinVT x TBottom = (NoChange, x)
    joinVT TTop x = (NoChange, VTop)
    joinVT x TTop = (SomeChange, VTop)
    joinVT x y | x == y = (NoChange, x)
    joinVT x y = (SomeChange, TTop)


      


-- aux: remvoe all records containing x, because x has been changed

-- transfer: define CSE lattice transfer function:  a expression (x op y) is available iff ((x,op,y),w) \in Lattice
-- concerns only RegAssignNode and LoadNode
exprIsAvail :: FwdTransfer Node CSEFact
exprIsAvail = mkFTransfer ft
  where
    ft :: Node e x -> CSEFact -> Fact x CSEFact
    ft (LIRLabelNode {}) f        = f
    ft (LIRRegAssignNode x (LIRBinExpr a op b)) = 
      do{ (m1,m2,m3) <- getMaps
        ; lab <- getCount
        ; let v1 = toMaybeV a m1
              v2 = toMaybeV b m1
        ; case (liftM2 M.lookup) ((liftM2 (\a b -> (a,op,b))) v1 v2) (Just m2) of
            Just (Just (JustV v')) ->
              do let m1' = M.insert x (JustV v') m1
                 setMaps (m1', m2, m3)
            otherwise ->  -- this covers several cases (Nothing, Just Nothing, and Just Just (not JustV)
              do let m1' = M.insert x (JustV lab) m1
                     m2' = M.insert ((toMaybeV a m1), op, (toMaybeV b m1)) (JustV lab) m2
                     m3' = M.insert ((toMaybeV a m1), op, (toMaybeV b m1)) (JustT lab) m3
                 setMaps (m1', m2', m3')
        }
    ft (LIRRegAssignNode x _) f   = f                                       -- x = _, no change
    ft (LIRRegOffAssignNode {}) f = f                                       -- write to a memory location indexed by the registers
    ft (LIRStoreNode {}) f        = f
    -- remove all records containing x
    ft (LIRLoadNode x _) f        = do { (m1,m2,m3) <- getMaps
                                       ; let m1' = M.insert x VTop m1
                                       ; setMaps (m1',m2,m3)}
    ft (LIRCallNode proc ret) f   = mkFactBase cseLattice [(proc, f), (ret, f)]
    ft (LIRCalloutNode {}) f      = f
    ft (LIREnterNode {}) f        = f
    ft (LIRRetNode {}) f          = mkFactBase cseLattice []
    ft (LIRIfNode expr tl fl) f   = mkFactBase cseLattice [(tl, f), (fl, f)]  -- if expr the jmp tl else jmp fl
    ft (LIRJumpLabelNode l) f     = mkFactBase cseLattice [(l, f)]        -- jmp l --> associate f with l 
    -- may need add more

-}

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
import Debug.Trace

import qualified Data.Map as M
----------------------------------------------------
-- this file contains CSE transfer/rewrite functions
-- currently it is local rewrite,  
-- would need to expand to global rewrite later



-- Note: 
-- currently only support
-- x = y binop z   -->  x = w 
-- not  x = y relop z, or x = uniop y

-- CSE data structure, maps (Opr, Op, Opr) key to Var 
{-
joinCSEFact :: CSEFact -> CSEFact -> (ChangeFlag, CSEFact)
joinCSEFact a b 
  | a == bottom && b /= bottom =
    (SomeChange, b)
  | a == b = 
    (NoChange a)
  | a /= bottom && b /= bottom = 
    join a b
  | otherwise =  -- otherwise, b is bottom
    (NoChange, a)
  where 
    join :: CSEFact -> CSEFact -> (ChangeFlag, CSEFact) -- nontrivial, nonequivalent facts
    join m1 m2 = 
      let (join' (rv1,vv1,vt1) (rv2,vv2,vt2) = 
      let m1 = M.unionWith joinRV rv1 rv2
          m2 = M.unionWith joinVV vv1 vv2
          m3 = M.unionWith joinVT vt1 vt2
      in
        ( or (map sc (M.toList m1 ++ M.toList m2 ++ M.toList m3))
        , (M.map snd m1, M.map snd m2, M.map snd m3))
      where
        sc keyvals = or (map ((== SomeChange) . fst) keyvals)

    joinRV :: MaybeV -> MaybeV -> MaybeV
    joinRV VBottom x = (SomeChange, x)
    joinRV x VBottom = (NoChange, x)
    joinRV VTop x = (NoChange, VTop)
    joinRV x VTop = (SomeChange, VTop)
    joinRV x y | x == y = (NoChange, x)
    joinRV x y = (SomeChange, VTop)

    joinVV = joinRV

    joinVT :: MaybeT -> MaybeT -> MaybeT
    joinVT TBottom x = (SomeChange, x)
    joinVT x TBottom = (NoChange, x)
    joinVT TTop x = (NoChange, VTop)
    joinVT x TTop = (SomeChange, VTop)
    joinVT x y | x == y = (NoChange, x)
    joinVT x y = (SomeChange, TTop)
-}
data CSEKey = CSEKey LIROperand LIRBinOp LIROperand
               deriving (Show, Eq, Ord)
type CSEData = LIRReg
type CSERecord = (CSEKey, CSEData)

type CSEFact = [CSERecord]


-- JOIN IS SEMANTICALLY INCORRECT, but works most of the time ;-p
-- join two CSE facts 
joinCSEFact :: CSEFact -> CSEFact -> (ChangeFlag, CSEFact)
joinCSEFact x  [] = (NoChange, trace "NO CHANGE: " x)
joinCSEFact [] [] = (NoChange, trace "NO CHANGE: []" [])
joinCSEFact [] x  = (SomeChange, trace ("SOME CHANGE:\n" ++ unlines (map show x)) x )
joinCSEFact x y = if x == y
                    then (NoChange, trace ("NOCHANGE:\n" ++ unlines (map show x))x)
                    else join x y
  where
    join :: CSEFact -> CSEFact -> (ChangeFlag, CSEFact)
    -- maybe we want something like this?
    -- this function converts the fact lists into maps, then
    -- intersects them, but stores the values for both in the result
    -- (hence intersectionWith), then picks out the ones where the two
    -- agree
    join x y = (SomeChange, foldl fix [] $ M.toList (M.intersectionWith (\a b -> (a,b)) (M.fromList x) (M.fromList y)))
      where 
        fix :: CSEFact -> (CSEKey, (CSEData, CSEData)) -> CSEFact
        fix fs x = if (fst . snd) x == (snd . snd) x then ((fst x), (fst . snd) x) : fs else fs
    -- your function
    join x y = let joined =  (concatMap (uncurry consolidate) inOther)
               in if joined == y || joined == x
                    then (NoChange, trace ("JOIN:\nNO CHANGE: " ++ unlines (map show joined))y)
                    else (SomeChange, trace ("JOIN:\nSOME CHANGE: " ++ unlines (map show joined))joined)
    
      where
        inOther :: [(CSERecord, Maybe LIRReg)]   -- the Maybe type indicates whether or not CSERecord (an element of x) is also present in y
        inOther = zip x (map (search y . fst) x) -- essentially, search through y for each element of x, and zip those results with x
        consolidate :: CSERecord -> Maybe LIRReg -> [CSERecord] -- consolidate (i.e. join) each element of x, with the result of the search for the same element in y on the right
        consolidate (k, d) (Just reg) = if (reg == d) then [(k, d)] else []   -- only keep an expression if it is available and equivalent in both sources; i.e. search found a meaningful result.
        consolidate (k, d) Nothing    = []                      -- search found nothing, so drop this expression
search :: CSEFact -> CSEKey -> Maybe LIRReg
search f k = 
  -- filter out only expression with key match
  let recs = Prelude.filter (\(x,y) -> if keyEq x k then True else False) f
  -- pick the first match
  in if recs == [] then Nothing else Just $ snd $ recs !! 0
keyEq :: CSEKey -> CSEKey -> Bool               -- FIX ME: an expression is equal iff operation is commutative  (SIMPLIFICATION; NOT ALWAYS TRUE)
keyEq (CSEKey op1 binop op2) (CSEKey op1' binop' op2')
    | (binop == binop') && (op1 == op1') && (op2 == op2') = True
    | otherwise                                           = False
    -- | (binop == binop') && (op1 == op2') && (op2 == op1') = True


-- define const lattice
constLattice :: DataflowLattice CSEFact
constLattice = DataflowLattice
  { factBottom = []
  , factJoin   = joinCSEFact }

-- aux: remvoe all records containing x, because x has been changed
varChanged :: CSEFact -> LIRReg -> CSEFact
varChanged f x = Prelude.filter ok f
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

-- transfer: define CSE lattice transfer function:  a expression (x op y) is available iff ((x,op,y),w) \in Lattice
-- concerns only RegAssignNode and LoadNode
exprIsAvail :: FwdTransfer Node CSEFact
exprIsAvail = mkFTransfer ft
  where
    ft :: Node e x -> CSEFact -> Fact x CSEFact
    ft (LIRLabelNode {}) f        = f
    ft (LIRRegAssignNode x (LIRBinExpr a op b)) f
                                  = ((CSEKey a op b), x) : (varChanged f x) -- a op b -> x
    ft (LIRRegAssignNode x _) f   = f                                       -- x = _, no change
    ft (LIRRegOffAssignNode {}) f = f                                       -- write to a memory location indexed by the registers
    ft (LIRStoreNode {}) f        = f
    ft (LIRLoadNode x _) f        = varChanged f x                          -- remove all records containing x
    ft (LIRCallNode proc ret) f   = mkFactBase constLattice [(proc, f), (ret, f)]
    ft (LIRCalloutNode {}) f      = f
    ft (LIREnterNode {}) f        = f
    ft (LIRRetNode {}) f          = mkFactBase constLattice []
    ft (LIRIfNode expr tl fl) f   = mkFactBase constLattice [(tl, f), (fl, f)]  -- if expr the jmp tl else jmp fl
    ft (LIRJumpLabelNode l) f     = mkFactBase constLattice [(l, f)]        -- jmp l --> associate f with l 
    -- may need add more

{-
-- rewrite: define constant folding rewrites
cse :: Monad m => FwdRewrite m Node CSEFact
cse begin = shallowFwdRw simp
  where
    simp :: forall m e x . (ShapeLifter e x, Monad m) => 
            Node e x -> CSEFact -> m (Maybe (Graph Node e x))
    simp node f = return $ liftM nodeToG $ s_node node
  
      where
        (m1,m2,m3) = runSM f (mkState begin)
        s_node :: Node e x -> Maybe (Node e x)
        -- x = lit op lit     
        s_node (LIRRegAssignNode x (LIRBinExpr o1 op o2)) = 
         let v1 = fromMaybe(M.lookup (toMaybeV o1 m1))
             v2 = fromMaybe(M.lookup (toMaybeV o2 m1))
         in case M.lookup (v1,op,v2) m3 of
              Nothing -> Nothing
              Just sr -> Just (LIRRegAssignNode x (LIROperExpr $ LIRRegOperand $ SREG sr))
        -- others do not need CSE
        s_node _ = Nothing
-}
-- rewrite: define constant folding rewrites
cse :: Monad m => FwdRewrite m Node CSEFact
cse  = shallowFwdRw simp
  where
    simp :: forall m e x . (ShapeLifter e x, Monad m) => 
            Node e x -> CSEFact -> m (Maybe (Graph Node e x))
    simp node f = return $ liftM nodeToG $ s_node (trace ("REWRITING NODE [" ++ show node ++ "] ~~~~~~~~~WITH FACTS~~~~~~~~~~~~ {\n" ++ unlines(map show f) ++ "}") node)
  
      where
        s_node :: Node e x -> Maybe (Node e x)
        -- x = lit op lit     
        s_node (LIRRegAssignNode reg (LIRBinExpr o1 binop o2)) = 
          case search f (CSEKey o1 binop o2) of
            Just result  -> trace ("search found! rewrote to " ++ show (LIRRegAssignNode reg $ LIROperExpr $ LIRRegOperand result)) (Just $ LIRRegAssignNode reg $ LIROperExpr $ LIRRegOperand result)
            Nothing -> trace "search failed!" Nothing

        -- others do not need CSE
        s_node _ = Nothing
{-
-- define fwd pass
csePass = FwdPass 
  { fpLattice = constLattice
  , fpTransfer = exprIsAvail
  , fpRewrite = cse } -- :: FwdRewrite LolMonad Node CSEFact}
-}
-- define fwd pass
csePass  = FwdPass 
  { fpLattice = constLattice
  , fpTransfer = exprIsAvail
  , fpRewrite = cse }
