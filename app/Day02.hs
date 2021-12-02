module Day02 (solve1, solve2) where

import Data.List (foldl')

data Step = Forward Int
          | Down    Int
          | Up      Int

data Position = Position Int Int Int -- Position horizontal depth aim

-- Input handling

input :: IO [Step]
input = parseInput <$> readFile "app/inputs/input02"
  where
    parseInput = map (toStep . words) . lines
    toStep ["forward", count] = Forward $ read count
    toStep ["down"   , count] = Down    $ read count
    toStep ["up"     , count] = Up      $ read count
    toStep _ = error "Unexpected input"

-- Solvers

solve1 :: IO ()
solve1 = print . result move1 =<< input

solve2 :: IO ()
solve2 = print . result move2 =<< input

result :: (Position -> Step -> Position) -> [Step] -> Int
result f = (\(Position h d _) -> h * d) . foldl' f (Position 0 0 0)

-- Logic

move1 :: Position -> Step -> Position
move1 (Position h d _) (Forward x) = Position (h + x)  d      0
move1 (Position h d _) (Down    x) = Position  h      (d + x) 0
move1 (Position h d _) (Up      x) = Position  h      (d - x) 0

move2 :: Position -> Step -> Position
move2 (Position h d a) (Forward x) = Position (h + x) (d + a*x)  a
move2 (Position h d a) (Down    x) = Position  h       d        (a + x)
move2 (Position h d a) (Up      x) = Position  h       d        (a - x)
