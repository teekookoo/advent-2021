module Day05 (solve1, solve2) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Text.Parsec

type VentLines = [((Int, Int), (Int, Int))]

-- Input handling

testInput :: IO VentLines
testInput = parseInput <$> readFile "app/inputs/input05_test"

input :: IO VentLines
input = parseInput <$> readFile "app/inputs/input05"

parseInput :: String -> VentLines
parseInput s = case parse ventLines "" s of Left  err -> error $ show err
                                            Right res -> res
  where
    ventLines = ventLine `endBy1` endOfLine <* eof
    ventLine = (,) <$> coord <* string " -> " <*> coord
    coord = (,) <$> int <* char ',' <*> int
    int = read <$> many1 digit

-- Solvers
  
solve1 :: IO ()
-- solve1 = print . M.size . M.filter (>= 2) . ventCounts . horzOrVert =<< testInput
solve1 = print . M.size . M.filter (>= 2) . ventCounts . horzOrVert =<< input
  where
    horzOrVert = filter (\((x, y), (x', y')) -> x == x' || y == y')

solve2 :: IO ()
-- solve2 = print . M.size . M.filter (>= 2) . ventCounts =<< testInput
solve2 = print . M.size . M.filter (>= 2) . ventCounts =<< input

-- Logic

ventCounts :: VentLines -> Map (Int, Int) Int
ventCounts = M.fromListWith (+) . flip zip (repeat 1) . concatMap coords
  where
    coords ((x, y), (x', y')) =
      let a  = x' - x
          b  = y' - y
          dx = signum a
          dy = signum b
      in [ (x + t*dx, y + t*dy) | t <- [0 .. abs a `max` abs b] ]
