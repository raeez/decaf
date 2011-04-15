module Decaf.Util.Prelude where

import Control.Monad
import Control.Monad.State

data StateMonad s a = SM {runSM :: s -> (a,s)}

instance Monad (StateMonad s) where
  m >>= f = SM ( \s -> let (a,s') = runSM m s
                       in runSM (f a) s')
  return f = SM (\s -> (f,s))

getST = SM (\s -> (s,s))
putST st = SM (\s -> ((),st))
