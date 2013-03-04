import Control.Monad.State

vax :: Integer -> (Integer, Integer)
vax seed0 = (seed1 `rem` 36, seed1)
  where seed1 = (69069 * seed0 + 1) `rem` 2^32

main :: IO ()
main = print $ runState (sequence (replicate 4 (state vax))) 6