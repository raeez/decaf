{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, 
 NoMonomorphismRestriction #-}

module Loligoptl.Combinators
  ( shallowFwdRw, deepFwdRw, deepFwdRw3, iterFwdRw
  , thenFwdRw
  , mkFTransfer , mkFTransfer3 , mkFRewrite, noFwdRewrite
  , mkBTransfer, mkBTransfer3, noBwdRewrite
  , thenBwdRw
  , deepBwdRw3, deepBwdRw, iterBwdRw
  , pairFwd, pairBwd, pairLattice
  , distributeFact
  , mkBRewrite
  )
where
  
import Loligoptl.Dataflow hiding (Graph)
import Loligoptl.Graph 
import Loligoptl.Fuel 
import Loligoptl.Label 
import Loligoptl.Collections
import Data.Maybe

import Control.Monad (liftM)

deepFwdRw3 :: FuelMonad m
           => (n C O -> f -> m (Maybe (Graph n C O)))
           -> (n O O -> f -> m (Maybe (Graph n O O)))
           -> (n O C -> f -> m (Maybe (Graph n O C)))
           -> (FwdRewrite m n f)
deepFwdRw :: forall m e x n f. (FuelMonad m)
           => (forall e x . (ShapeLifter e x)
           =>  n e x -> f -> m (Maybe (Graph n e x)))
           -> FwdRewrite m n f
deepFwdRw3 f m l = iterFwdRw $ mkFRewrite3 f m l
deepFwdRw f = deepFwdRw3 f f f

-- N.B. rw3, rw3', and rw3a are triples of functions.
-- But rw and rw' are single functions.
-- @ start comb1.tex
thenFwdRw :: forall m n f. Monad m 
          => FwdRewrite m n f 
          -> FwdRewrite m n f 
          -> FwdRewrite m n f
-- @ end comb1.tex
thenFwdRw rw3 rw3' = wrapFR2 thenrw rw3 rw3'
 where
  thenrw :: forall m1 e x t t1.
            Monad m1 =>
            (t -> t1 -> m1 (Maybe (Graph n e x, FwdRewrite m n f)))
            -> (t -> t1 -> m1 (Maybe (Graph n e x, FwdRewrite m n f)))
            -> t
            -> t1
            -> m1 (Maybe (Graph n e x, FwdRewrite m n f))
  thenrw rw rw' n f = rw n f >>= fwdRes
     where fwdRes Nothing   = rw' n f
           fwdRes (Just gr) = return $ Just $ fadd_rw rw3' gr

iterFwdRw :: forall m n f. Monad m 
          => FwdRewrite m n f 
          -> FwdRewrite m n f
iterFwdRw rw3 = wrapFR iter rw3
 where iter :: forall a m1 m2 e x t.
               (Monad m2, Monad m1) =>
               (t -> a -> m1 (m2 (Graph n e x, FwdRewrite m n f)))
               -> t
               -> a
               -> m1 (m2 (Graph n e x, FwdRewrite m n f))
       iter rw n = (liftM $ liftM $ fadd_rw (iterFwdRw rw3)) . rw n

-- | Function inspired by 'rew' in the paper
frewrite_cps :: Monad m
             => ((Graph n e x, FwdRewrite m n f) -> m a)
             -> m a
             -> (forall e x . n e x -> f -> m (Maybe (Graph n e x, FwdRewrite m n f)))
             -> n e x
             -> f
             -> m a
frewrite_cps j n rw node f =
    do mg <- rw node f
       case mg of Nothing -> n
                  Just gr -> j gr



-- | Function inspired by 'add' in the paper
fadd_rw :: Monad m
       => FwdRewrite m n f
       -> (Graph n e x, FwdRewrite m n f)
       -> (Graph n e x, FwdRewrite m n f)
fadd_rw rw2 (g, rw1) = (g, rw1 `thenFwdRw` rw2)

-- | make a  (n e x -> f -> m (Maybe (Graph n e x))) function
--   a rewrite function
shallowFwdRw :: forall m e x n f . (Monad m) => 
                (forall e x . (ShapeLifter e x) => 
                 n e x -> f -> m (Maybe (Graph n e x))) -> 
                FwdRewrite m n f

shallowFwdRw rw = FwdRewrite3 { getFwdRewrite3 = (rw', rw', rw') }
                  where
                    rw' :: forall e x . (ShapeLifter e x) => 
                           n e x -> f -> m (Maybe (Graph n e x, FwdRewrite m n f))
                    rw' n f = do
                      x <- rw n f
                      case x of
                        Nothing -> return Nothing
                        Just x' -> return $ Just (x', emptyRw)
                    
                    emptyRw :: forall m n f . (Monad m) => FwdRewrite m n f
                    emptyRw = FwdRewrite3 { getFwdRewrite3 = (rw'', rw'', rw'') }
                    
                    rw'' :: forall m n f e x . (Monad m) => 
                            n e x -> f -> m (Maybe (Graph n e x, FwdRewrite m n f))
                    rw'' _ _ = return Nothing




wrapFR :: (forall e x. (n  e x -> f  -> m  (Maybe (Graph n  e x, FwdRewrite m  n  f )))
                    -> (n' e x -> f' -> m' (Maybe (Graph n' e x, FwdRewrite m' n' f')))
          )
            -- ^ This argument may assume that any function passed to it
            -- respects fuel, and it must return a result that respects fuel.
       -> FwdRewrite m  n  f 
       -> FwdRewrite m' n' f'      -- see Note [Respects Fuel]
wrapFR wrap (FwdRewrite3 (f, m, l)) = FwdRewrite3 (wrap f, wrap m, wrap l)
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

-- N.B. rw3, rw3', and rw3a are triples of functions.
-- But rw and rw' are single functions.
-- @ start comb1.tex

mkFTransfer3 :: (n C O -> f -> f)
             -> (n O O -> f -> f)
             -> (n O C -> f -> FactBase f)
             -> FwdTransfer n f
mkFTransfer3 f m l = FwdTransfer3 (f, m, l)

mkFTransfer :: (forall e x . n e x -> f -> Fact x f) -> FwdTransfer n f
mkFTransfer f = FwdTransfer3 (f, f, f)

-- | Functions passed to 'mkFRewrite3' should not be aware of the fuel supply.
-- The result returned by 'mkFRewrite3' respects fuel.
mkFRewrite3 :: forall m n f. FuelMonad m
            => (n C O -> f -> m (Maybe (Graph n C O)))
            -> (n O O -> f -> m (Maybe (Graph n O O)))
            -> (n O C -> f -> m (Maybe (Graph n O C)))
            -> FwdRewrite m n f
mkFRewrite3 f m l = FwdRewrite3 (lift f, lift m, lift l)
  where lift :: forall t t1 a. (t -> t1 -> m (Maybe a)) -> t -> t1 -> m (Maybe (a, FwdRewrite m n f))
        lift rw node fact = liftM (liftM asRew) (withFuel =<< rw node fact)
        asRew :: forall t. t -> (t, FwdRewrite m n f)
        asRew g = (g, noFwdRewrite)

mkFRewrite :: forall m e x n f. (FuelMonad m)
           => (forall e x . (ShapeLifter e x)
           =>  n e x -> f -> m (Maybe (Graph n e x)))
           -> FwdRewrite m n f
mkFRewrite f = mkFRewrite3 f f f

noFwdRewrite :: Monad m => FwdRewrite m n f
noFwdRewrite = FwdRewrite3 (noRewrite, noRewrite, noRewrite)

noRewrite :: Monad m => a -> b -> m (Maybe c)
noRewrite _ _ = return Nothing

distributeFact :: NonLocal n => n O C -> f -> FactBase f
distributeFact n f = mapFromList [ (l, f) | l <- successors n ]
   -- because the same fact goes out on every edge,
   -- there's no need for 'mkFactBase' here.
   --
----------------------------------------------------------------

deepBwdRw3 :: FuelMonad m
           => (n C O -> f          -> m (Maybe (Graph n C O)))
           -> (n O O -> f          -> m (Maybe (Graph n O O)))
           -> (n O C -> FactBase f -> m (Maybe (Graph n O C)))
           -> (BwdRewrite m n f)
deepBwdRw  :: FuelMonad m
           => (forall e x . n e x -> Fact x f -> m (Maybe (Graph n e x)))
           -> BwdRewrite m n f
deepBwdRw3 f m l = iterBwdRw $ mkBRewrite3 f m l
deepBwdRw  f = deepBwdRw3 f f f


thenBwdRw :: forall m n f. Monad m => BwdRewrite m n f -> BwdRewrite m n f -> BwdRewrite m n f
thenBwdRw rw1 rw2 = wrapBR2 f rw1 rw2
  where f :: forall t t1 t2 m1 e x.
             Monad m1 =>
             t
             -> (t1 -> t2 -> m1 (Maybe (Graph n e x, BwdRewrite m n f)))
             -> (t1 -> t2 -> m1 (Maybe (Graph n e x, BwdRewrite m n f)))
             -> t1
             -> t2
             -> m1 (Maybe (Graph n e x, BwdRewrite m n f))
        f _ rw1 rw2' n f = do
          res1 <- rw1 n f
          case res1 of
            Nothing -> rw2' n f
            Just gr -> return $ Just $ badd_rw rw2 gr

iterBwdRw :: forall m n f. Monad m => BwdRewrite m n f -> BwdRewrite m n f
iterBwdRw rw = wrapBR f rw
  where f :: forall t m1 m2 e x t1 t2.
             (Monad m2, Monad m1) =>
             t
             -> (t1 -> t2 -> m1 (m2 (Graph n e x, BwdRewrite m n f)))
             -> t1
             -> t2
             -> m1 (m2 (Graph n e x, BwdRewrite m n f))
        f _ rw' n f = liftM (liftM (badd_rw (iterBwdRw rw))) (rw' n f)

-- | Function inspired by 'add' in the paper
badd_rw :: Monad m
       => BwdRewrite m n f
       -> (Graph n e x, BwdRewrite m n f)
       -> (Graph n e x, BwdRewrite m n f)
badd_rw rw2 (g, rw1) = (g, rw1 `thenBwdRw` rw2)


pairFwd :: forall m n f f'. Monad m
        => FwdPass m n f
        -> FwdPass m n f' 
        -> FwdPass m n (f, f')
pairFwd pass1 pass2 = FwdPass lattice transfer rewrite
  where
    lattice = pairLattice (fp_lattice pass1) (fp_lattice pass2)
    transfer = mkFTransfer3 (tf tf1 tf2) (tf tm1 tm2) (tfb tl1 tl2)
      where
        tf :: forall t t1 t2 t3 t4.
              (t4 -> t -> t2) -> (t4 -> t1 -> t3) -> t4 -> (t, t1) -> (t2, t3)
        tf  t1 t2 n (f1, f2) = (t1 n f1, t2 n f2)
        tfb t1 t2 n (f1, f2) = mapMapWithKey withfb2 fb1
          where fb1 = t1 n f1
                fb2 = t2 n f2
                withfb2 :: forall t. Label -> t -> (t, f')
                withfb2 l f = (f, fromMaybe bot2 $ lookupFact l fb2)
                bot2 = fact_bot (fp_lattice pass2)
        (tf1, tm1, tl1) = getFTransfer3 (fp_transfer pass1)
        (tf2, tm2, tl2) = getFTransfer3 (fp_transfer pass2)
    rewrite = lift fst (fp_rewrite pass1) `thenFwdRw` lift snd (fp_rewrite pass2) 
      where
        lift :: forall f m' n' f'.
                Monad m' =>
                (f' -> f) -> FwdRewrite m' n' f -> FwdRewrite m' n' f'
        lift proj = wrapFR project
          where project :: forall m m1 t t1.
                          (Monad m1, Monad m) =>
                          (t1 -> f -> m (m1 (t, FwdRewrite m' n' f)))
                          -> t1
                          -> f'
                          -> m (m1 (t, FwdRewrite m' n' f'))
                project rw = \n pair -> liftM (liftM repair) $ rw n (proj pair)
                repair :: forall t.
                          (t, FwdRewrite m' n' f) -> (t, FwdRewrite m' n' f')
                repair (g, rw') = (g, lift proj rw')

pairBwd :: forall m n f f' . 
           Monad m => BwdPass m n f -> BwdPass m n f' -> BwdPass m n (f, f')
pairBwd pass1 pass2 = BwdPass lattice transfer rewrite
  where
    lattice = pairLattice (bp_lattice pass1) (bp_lattice pass2)
    transfer = mkBTransfer3 (tf tf1 tf2) (tf tm1 tm2) (tfb tl1 tl2)
      where
        tf :: (t4 -> t -> t2) -> (t4 -> t1 -> t3) -> t4 -> (t, t1) -> (t2, t3)
        tf  t1 t2 n (f1, f2) = (t1 n f1, t2 n f2)
        tfb :: IsMap map =>
               (t2 -> map a -> t)
               -> (t2 -> map b -> t1)
               -> t2
               -> map (a, b)
               -> (t, t1)
        tfb t1 t2 n fb = (t1 n $ mapMap fst fb, t2 n $ mapMap snd fb)
        (tf1, tm1, tl1) = getBTransfer3 (bp_transfer pass1)
        (tf2, tm2, tl2) = getBTransfer3 (bp_transfer pass2)
    rewrite = lift fst (bp_rewrite pass1) `thenBwdRw` lift snd (bp_rewrite pass2) 
      where
       lift :: forall f1 .
                ((f, f') -> f1) -> BwdRewrite m n f1 -> BwdRewrite m n (f, f')
       lift proj = wrapBR project
        where project :: forall e x . Shape x 
               -> (n e x ->
                       Fact x f1     -> m (Maybe (Graph n e x, BwdRewrite m n f1)))
               -> (n e x ->
                       Fact x (f,f') -> m (Maybe (Graph n e x, BwdRewrite m n (f,f'))))
              project Open = 
                 \rw n pair -> liftM (liftM repair) $ rw n (       proj pair)
              project Closed = 
                 \rw n pair -> liftM (liftM repair) $ rw n (mapMap proj pair)
              repair :: forall t.
                        (t, BwdRewrite m n f1) -> (t, BwdRewrite m n (f, f'))
              repair (g, rw') = (g, lift proj rw')
                -- XXX specialize repair so that the cost
                -- of discriminating is one per combinator not one
                -- per rewrite

pairLattice :: forall f f' .
               DataflowLattice f -> DataflowLattice f' -> DataflowLattice (f, f')
pairLattice l1 l2 =
  DataflowLattice
    { fact_name = fact_name l1 ++ " x " ++ fact_name l2
    , fact_bot  = (fact_bot l1, fact_bot l2)
    , fact_join = join
    }
  where
    join lbl (OldFact (o1, o2)) (NewFact (n1, n2)) = (c', (f1, f2))
      where (c1, f1) = fact_join l1 lbl (OldFact o1) (NewFact n1)
            (c2, f2) = fact_join l2 lbl (OldFact o2) (NewFact n2)
            c' = case (c1, c2) of
                   (NoChange, NoChange) -> NoChange
                   _                    -> SomeChange

-- | Functions passed to 'mkBRewrite3' should not be aware of the fuel supply.
-- The result returned by 'mkBRewrite3' respects fuel.
mkBRewrite3 :: forall m n f. FuelMonad m
            => (n C O -> f          -> m (Maybe (Graph n C O)))
            -> (n O O -> f          -> m (Maybe (Graph n O O)))
            -> (n O C -> FactBase f -> m (Maybe (Graph n O C)))
            -> BwdRewrite m n f
mkBRewrite3 f m l = BwdRewrite3 (lift f, lift m, lift l)
  where lift :: forall t t1 a. (t -> t1 -> m (Maybe a)) -> t -> t1 -> m (Maybe (a, BwdRewrite m n f))
        lift rw node fact = liftM (liftM asRew) (withFuel =<< rw node fact)
        asRew :: t -> (t, BwdRewrite m n f)
        asRew g = (g, noBwdRewrite)

noBwdRewrite :: Monad m => BwdRewrite m n f
noBwdRewrite = BwdRewrite3 (noRewrite, noRewrite, noRewrite)

mkBTransfer3 :: (n C O -> f -> f) -> (n O O -> f -> f) ->
                (n O C -> FactBase f -> f) -> BwdTransfer n f
mkBTransfer3 f m l = BwdTransfer3 (f, m, l)

mkBTransfer :: (forall e x . n e x -> Fact x f -> f) -> BwdTransfer n f
mkBTransfer f = BwdTransfer3 (f, f, f)

wrapBR :: (forall e x .
                Shape x 
             -> (n  e x -> Fact x f  -> m  (Maybe (Graph n  e x, BwdRewrite m  n  f )))
             -> (n' e x -> Fact x f' -> m' (Maybe (Graph n' e x, BwdRewrite m' n' f')))
          )
            -- ^ This argument may assume that any function passed to it
            -- respects fuel, and it must return a result that respects fuel.
       -> BwdRewrite m  n  f 
       -> BwdRewrite m' n' f'      -- see Note [Respects Fuel]
wrapBR wrap (BwdRewrite3 (f, m, l)) = 
  BwdRewrite3 (wrap Open f, wrap Open m, wrap Closed l)

wrapBR2 :: (forall e x . Shape x
            -> (n1 e x -> Fact x f1 -> m1 (Maybe (Graph n1 e x, BwdRewrite m1 n1 f1)))
            -> (n2 e x -> Fact x f2 -> m2 (Maybe (Graph n2 e x, BwdRewrite m2 n2 f2)))
            -> (n3 e x -> Fact x f3 -> m3 (Maybe (Graph n3 e x, BwdRewrite m3 n3 f3))))
            -- ^ This argument may assume that any function passed to it
            -- respects fuel, and it must return a result that respects fuel.
        -> BwdRewrite m1 n1 f1
        -> BwdRewrite m2 n2 f2
        -> BwdRewrite m3 n3 f3      -- see Note [Respects Fuel]
wrapBR2 wrap2 (BwdRewrite3 (f1, m1, l1)) (BwdRewrite3 (f2, m2, l2)) =
    BwdRewrite3 (wrap2 Open f1 f2, wrap2 Open m1 m2, wrap2 Closed l1 l2)

-- | Functions passed to 'mkBRewrite' should not be aware of the fuel supply.
-- The result returned by 'mkBRewrite' respects fuel.
mkBRewrite :: FuelMonad m 
           => (forall e x . n e x -> Fact x f -> m (Maybe (Graph n e x)))
           -> BwdRewrite m n f
mkBRewrite f = mkBRewrite3 f f f
