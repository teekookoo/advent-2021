module Day01 (solve1, solve2) where

import Data.List (tails)

input :: IO [Int]
input = map read . lines <$> readFile "app/inputs/input01"

solve1 :: IO ()
solve1 = print . increasedCount 1 =<< input

solve2 :: IO ()
solve2 = print . increasedCount 3 =<< input

increasedCount :: (Ord a, Integral b) => Int -> [a] -> b
increasedCount offset xs = foldr f 0 . zip xs $ drop offset xs
  where f (x, y) c | x < y     = c + 1
                   | otherwise = c
