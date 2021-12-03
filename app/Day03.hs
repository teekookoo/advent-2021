module Day03 (solve1, solve2) where

import Control.Arrow (second)
import Data.List (foldl', transpose)

type Bit    = Bool
type Binary = [Bit]

--

input :: IO [Binary]
input = parseInput <$> readFile "app/inputs/input03"
  where
    parseInput = map (map toBit) . lines
    toBit '0' = False
    toBit '1' = True

--

solve1 :: IO ()
solve1 = print . powerConsumption =<< input

solve2 :: IO ()
solve2 = print . lifeSupportRating =<< input

--

mostCommonBit :: Binary -> Bit
mostCommonBit binary
  | count == 0 = True
  | otherwise  = count > 0
  where
    count = foldl' (\c b -> if b then c+1 else c-1) 0 binary

leastCommonBit :: Binary -> Bit
leastCommonBit = not . mostCommonBit

toDecimal :: Binary -> Int
toDecimal = sum . zipWith f [0..] . reverse
  where
    f k True  = 2^k
    f k False = 0

powerConsumption :: [Binary] -> Int
powerConsumption diagnostic = toDecimal gamma * toDecimal epsilon
  where
    gamma   = map mostCommonBit $ transpose diagnostic
    epsilon = map not gamma

findRating :: (Binary -> Bit) -> [Binary] -> Binary
findRating test diagnostic = diagnostic !! go (zip [0..] diagnostic)
  where
    go [(i, _)] = i
    go bins     = go keep
      where
        keep = map (second $ drop 1) $ filter ((== testBit) . head . snd) bins
        testBit = test $ map (head . snd) bins

lifeSupportRating :: [Binary] -> Int
lifeSupportRating diagnostic = toDecimal oxygen * toDecimal co2
  where
    oxygen = findRating mostCommonBit  diagnostic
    co2    = findRating leastCommonBit diagnostic
