{-# LANGUAGE OverloadedStrings #-}

module Day17 (solve1, solve2) where

import Text.Megaparsec
import Text.Megaparsec.Char

import Input (Parser, parseFile')

type Area     = (Int, Int, Int, Int) -- xmin xmax ymin ymax
type Velocity = (Int, Int)           -- vx vy

-- Input

testInput :: IO Area
testInput = parseFile' parser "app/inputs/input17_test"

input :: IO Area
input = parseFile' parser "app/inputs/input17"

parser :: Parser Area
parser = (,,,) <$> (chunk "target area: x=" *> int) <* chunk ".." <*> int
               <*   chunk ", y="           <*> int  <* chunk ".." <*> int
               <* optional eol <* eof
  where
    int = fmap read $ (:) <$> single '-' <*> some digitChar <|> some digitChar

-- Solvers

-- Part 1 solved with pen and paper :-)
solve1 :: IO ()
-- solve1 = print . (\(_, _, ymin, _) -> (ymin + 1) * ymin `div` 2) =<< testInput
solve1 = print . (\(_, _, ymin, _) -> (ymin + 1) * ymin `div` 2) =<< input

solve2 :: IO ()
-- solve2 = print . length . validVelocities =<< testInput
solve2 = print . length . validVelocities =<< input

-- Logic

validVelocities :: Area -> [Velocity]
validVelocities (xmin, xmax, ymin, ymax) = filter isValid searchSpace
  where
    isValid (v0x, v0y) =
      let (tMinX, tMaxX) = tBoundsX v0x
          (tMinY, tMaxY) = tBoundsY v0y
       in (tMinX `max` tMinY) <= (tMaxX `min` tMaxY)

    searchSpace = do
      v0x <- [ceiling $ (-1 + sqrt (fromIntegral $ 1 + 8 * xmin)) / 2 .. xmax]
      v0y <- [ymin .. -ymin - 1]
      return (v0x, v0y)

    tBoundsX :: Int -> (Int, Int)
    tBoundsX v0 =
      let b = fromIntegral v0 + 1/2
          d = b^2 - fromIntegral (2*xmax)
          ub = if d < 0
             then maxBound
             else floor $ b - sqrt d
          lb  = ceiling $ b - sqrt (b^2 - fromIntegral (2*xmin))
       in (lb, ub)

    tBoundsY :: Int -> (Int, Int)
    tBoundsY v0 =
      let b = fromIntegral v0 + 1/2
       in ( ceiling $ b + sqrt (b^2 - fromIntegral (2*ymax))
          , floor   $ b + sqrt (b^2 - fromIntegral (2*ymin)) )
