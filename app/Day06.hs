module Day06 (solve1, solve2) where

import           Control.Applicative (liftA)
import           Data.Char           (digitToInt)
import qualified Data.IntMap.Lazy                 as LM
import qualified Data.IntMap.Strict               as SM
import           Data.Maybe          (mapMaybe)
import           Text.Parsec

-- Inputs

testInput :: IO [Int]
testInput = pure [3,4,3,1,2]

input :: IO [Int]
input = parseInput <$> readFile "app/inputs/input06"
  where
    parseInput :: String -> [Int]
    parseInput s = case parse ints "" s of Left err  -> error $ show err
                                           Right res -> res
    ints = (digitToInt <$> digit) `sepBy1` char ',' <* endOfLine <* eof

-- Solvers

solve :: Int -> [Int] -> Int
solve d = either error sum . catEithers . map (fmap (+ 1) . descendantsAfter d)

solve1 :: IO ()
solve1 = print . solve 80
  =<< input
  -- =<< testInput

solve2 :: IO ()
solve2 = print . solve 256
  =<< input
  -- =<< testInput

-- Logic

descendantsAfter :: Integral a => Int -> Int -> Either String a
descendantsAfter d timer = case SM.lookup timer restrictedCounts of
  Nothing -> Left $ "Error: invalid timer value: " ++ show timer
  Just n  -> Right n
  where
    restrictedCounts = SM.fromList [ (t, counts LM.! i d t) | t <- [0 .. 8] ]
    counts = LM.fromAscList [ (i n t, f n t) | n <- [0 .. d], t <- [0 .. 8] ]
    i n t = n * 9 + t
    f n t
      | n <= t    = 0
      | otherwise = 1 + counts LM.! i (n - t - 1) 6
                      + counts LM.! i (n - t - 1) 8

catEithers :: [Either a b] -> Either a [b]
catEithers = foldr f (Right [])
  where
    f (Left  x)  _         = Left x
    f (Right x) (Right xs) = Right $ x:xs
    f (Right _) (Left  _ ) = undefined -- impossible due to short circuiting
