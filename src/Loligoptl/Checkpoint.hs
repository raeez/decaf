{-# LANGUAGE TypeFamilies #-}

module Loligoptl.Checkpoint
  ( CheckpointMonad(..)
  )
where

-- | Obeys the following law:
-- for all @m@ 
-- @
--    do { s <- checkpoint; m; restart s } == return ()
-- @
class Monad m => CheckpointMonad m where
  type Checkpoint m
  checkpoint :: m (Checkpoint m)
  restart    :: Checkpoint m -> m () 

