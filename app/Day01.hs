module Day01 (solve1, solve2) where

import Data.List (tails)

input :: IO [Int]
input = map read . lines <$> readFile "app/inputs/input01"

solve1 :: IO ()
solve1 = input >>= print . increasedCount

solve2 :: IO ()
solve2 = input >>= print . increasedCount . sliding sum 3

increasedCount :: [Int] -> Int
increasedCount xs = foldr f 0 . zip xs $ drop 1 xs
  where
    f (x, y) c
      | x < y     = c + 1
      | otherwise = c

sliding :: ([a] -> b) -> Int -> [a] -> [b]
sliding f n xs = map (f . take n) . take m $ tails xs
  where
    m = length xs - n + 1
