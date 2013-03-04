import Control.Monad.State

vax :: Integer -> (Integer, Integer)
vax seed0 = (seed1 `rem` 36, seed1)
  where seed1 = (69069 * seed0 + 1) `rem` 2^32

generate :: Integer -> Int -> [Integer]
generate seed n = fst $ runState (replicateM n $ state vax) seed

is_seed :: Integer -> [Integer] -> Bool
is_seed seed sample = generate seed (length sample) == sample

find_seed :: [Integer] -> Integer
find_seed sample = head [seed | seed <- [0..], is_seed seed sample]

find_next :: [Integer] -> Int -> [Integer]
find_next sample n = drop (length sample) $ generate (find_seed sample) $ length sample + n

main :: IO ()
main = print $ find_next [34, 27, 16, 1, 34, 31, 24, 17, 34, 35, 16, 13] 3
--main = print $ drop 12 $ generate 1628408773 15