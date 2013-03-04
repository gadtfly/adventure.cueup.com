import Control.Monad.State
import Debug.Trace

vax :: Integer -> (Integer, Integer)
vax seed0 = (seed1 `rem` 36, seed1)
  where seed1 = (69069 * seed0 + 1) `rem` 2^32

generate :: Integer -> Int -> [Integer]
generate seed n = fst $ runState (replicateM n $ state vax) seed

is_seed :: Integer -> [Integer] -> Bool
is_seed seed sample = generate seed (length sample) == sample

find_next :: [Integer] -> Int -> [Integer]
find_next sample n = drop (length sample) $ generate (find_seed sample) $ length sample + n


-- Monitor search progress

find_seed :: [Integer] -> Integer
find_seed sample = print_seed $ head [seed | seed <- [0..], is_seed (debug_progress seed) sample]

debug_progress :: Integer -> Integer
debug_progress seed | x == 0.5  = trace ("Testing: 2^" ++ show n) seed
                    | otherwise = seed
  where
    x = significand $ fromIntegral seed
    n = exponent    $ fromIntegral seed

print_seed :: Integer -> Integer
print_seed seed = trace ("Seed is: " ++ show seed) seed


-- Run search

main :: IO ()
main = print $ find_next [34, 27, 16, 1, 34, 31, 24, 17, 34, 35, 16, 13] 3
-- Seed is: 1628408773