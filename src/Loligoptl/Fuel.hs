module Loligoptl.Fuel
  ( FuelMonad
  , LolMonad(..)
  , withFuel
  , mkInfiniteFuel, mkFuel
  )
where
import Control.Monad
import Control.Monad.State

type Fuel = Int
class Monad m => FuelMonad m where
  getFuel :: m Fuel
  setFuel :: Fuel -> m()

withFuel :: FuelMonad m => Maybe a -> m (Maybe a)
withFuel Nothing = return Nothing
withFuel (Just a) = do f <- getFuel
                       if f == 0
                         then return Nothing
                         else setFuel (f-1) >> return (Just a)


data LolState a = LS { fuel :: Fuel, userState :: a }
mkInfiniteFuel = LS (-1)
mkFuel i = LS i
newtype LolMonad st a = LM { runLFM :: LolState st -> (a, LolState st) }

instance Monad (LolMonad st) where
    return a = LM (\s -> (a, s))
    m >>= f = LM (\s ->
                    let (a, s') = runLFM m s
                    in runLFM (f a) s')

instance FuelMonad (LolMonad st) where
  getFuel = LM (\s -> (fuel s, s))
  setFuel f = LM (\s -> ((), s {fuel = f}))

getState :: LolMonad st st
getState = LM(\s -> (userState s, s))
setState :: st -> LolMonad st ()
setState st = LM(\s -> ((), s{userState = st}))