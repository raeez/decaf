{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, 
 NoMonomorphismRestriction #-}

module Loligoptl.Combinators
where
  
import Loligoptl.Dataflow
import Loligoptl.Graph 
import Loligoptl.Fuel 
import Loligoptl.Label 
import Data.Maybe




-- generate transfer function
mkFTransfer :: (forall e x. n e x -> f -> Fact x f) -> FwdTransfer n f
mkFTransfer t = FwdTransfer3 { getFwdTransfer3 = (t, t, t) }




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
                           n e x -> f -> m (Maybe (FwdRev m n f e x))
                    rw' n f = do
                      x <- rw n f
                      case x of
                        Nothing -> return Nothing
                        Just x' -> return $ Just $ FwdRev x' emptyRw
                    
                    emptyRw :: forall m n f . (Monad m) => FwdRewrite m n f
                    emptyRw = FwdRewrite3 { getFwdRewrite3 = (rw'', rw'', rw'') }
                    
                    rw'' :: forall m n f e x . (Monad m) => 
                            n e x -> f -> m (Maybe (FwdRev m n f e x))
                    rw'' _ _ = return Nothing





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



