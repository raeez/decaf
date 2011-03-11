module Decaf.Util.Prelude where

manyList :: Integer -> a -> [a]
manyList n x = map (\_ -> x) [1..n]

-- EQUIVALENT TO Prelude.sequence
bindAccum :: (Monad m) => [m a] -> (m [a])
bindAccum ms = bahelp [] ms
    where 
      bahelp res ms =
          case ms of
            [] -> return res
            _ -> do t <- (head ms)
                    bahelp (res++[t]) (tail ms)