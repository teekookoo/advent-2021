module Day07 (solve1, solve2) where

import           Control.Arrow ((&&&))
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed          as V
import           Text.Parsec

-- Input handling

testInput :: IO (Vector Int)
testInput = pure $ V.fromList [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

input :: IO (Vector Int)
input = parseInput <$> readFile "app/inputs/input07"
  where
    parseInput = either (error . show) V.fromList . parse ints ""
    ints = sepBy1 (read <$> many1 digit) (char ',') <* endOfLine <* eof

-- Solvers

solve :: (Vector Int -> Int -> Int) -> Vector Int -> Int
solve cost = V.minimum . uncurry V.map . (cost &&& domain)

solve1 :: IO ()
-- solve1 = testInput >>= print . solve cost1
solve1 = input >>= print . solve cost1

solve2 :: IO ()
-- solve2 = testInput >>= print . solve cost2
solve2 = input >>= print . solve cost2

-- Logic

domain :: Vector Int -> Vector Int
domain = uncurry V.enumFromN
       . (\(a, b) -> (a, 1 + b - a))
       . (V.minimum &&& V.maximum)

cost :: (Int -> Int) -> Vector Int -> Int -> Int
cost f xs p = V.sum . V.map (f . abs . subtract p) $ xs
    
cost1 :: Vector Int -> Int -> Int
cost1 = cost id

cost2 :: Vector Int -> Int -> Int
cost2 = cost $ \x -> (x + 1) * x `div` 2
