module Day06 (solve1, solve2) where

import           Control.Applicative (liftA)
import           Data.Char           (digitToInt)
import qualified Data.IntMap.Lazy                 as LM
import qualified Data.IntMap.Strict               as SM
import           Data.Maybe          (mapMaybe)
import           Text.Parsec

-- Inputs

testInput :: IO [Int]
testInput = pure $ [3,4,3,1,2]

input :: IO [Int]
input = parseInput <$> readFile "app/inputs/input06"
  where
    parseInput :: String -> [Int]
    parseInput s = case parse ints "" s of Left err  -> error $ show err
                                           Right res -> res
    ints = (digitToInt <$> digit) `sepBy1` char ',' <* endOfLine <* eof

-- Solvers

solve1 :: IO ()
solve1 = print . sum . mapMaybe (liftA (1 +) . descendantsAfter 80)
  =<< input
  -- =<< testInput

solve2 :: IO ()
solve2 = print . sum . mapMaybe (liftA (1 +) . descendantsAfter 256)
  =<< input
  -- =<< testInput

-- Logic

descendantsAfter :: Integral a => Int -> Int -> Maybe a
descendantsAfter d = flip SM.lookup restrictedCounts
  where
    restrictedCounts = SM.fromList [ (c, counts LM.! i d c) | c <- [0 .. 8] ]
    counts = LM.fromAscList [ (i n c, f n c) | n <- [0 .. d], c <- [0 .. 8] ]
    i n c = n * 9 + c
    f n c
      | n <= c    = 0
      | otherwise = 1 + counts LM.! i (n - c - 1) 6
                      + counts LM.! i (n - c - 1) 8
