{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, 
 NoMonomorphismRestriction #-}

module Loligoptl.Combinators
  ( mkFTransfer
  , shallowFwdRw
  , thenFwdRw
  , mkFTransfer , mkFTransfer3 , noFwdRewrite
  )
where
  
import Loligoptl.Dataflow
import Loligoptl.Graph 
import Loligoptl.Fuel 
import Loligoptl.Label 
import Data.Maybe

import Control.Monad (liftM)
-- create deep rewrite function
-- DOES NOT COMPILE
{-
deepFwdRw :: forall m e x n f. (Monad m) => (forall e x. n e x -> f -> m (Maybe (Graph n e x))) -> FwdRewrite m n f
deepFwdRw rw = FwdRewrite3 { getFwdRewrite3 = (rw', rw', rw') }
              where 
                rw' :: forall e x. n e x -> f -> m (Maybe (FwdRev m n f e x))
                rw' n f = do 
                  x <- rw n f 
                  case x of 
                    Nothing -> return Nothing
                    Just x' -> return $ Just $ FwdRev x' $ deepFwdRw rw
-}                    





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

    


-- | Function inspired by 'add' in the paper
fadd_rw :: Monad m
       => FwdRewrite m n f
       -> (Graph n e x, FwdRewrite m n f)
       -> (Graph n e x, FwdRewrite m n f)
fadd_rw rw2 (g, rw1) = (g, rw1 `thenFwdRw` rw2)


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

noFwdRewrite :: Monad m => FwdRewrite m n f
noFwdRewrite = FwdRewrite3 (noRewrite, noRewrite, noRewrite)

noRewrite :: Monad m => a -> b -> m (Maybe c)
noRewrite _ _ = return Nothing

