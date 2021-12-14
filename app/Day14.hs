{-# LANGUAGE OverloadedStrings #-}

module Day14 (solve1, solve2) where

import Control.Arrow ((&&&))
import Data.Foldable (foldl')
import Data.Map      (Map, (!))

import Input (Parser, parseFile')

import qualified Data.Map             as M
import qualified Data.Set             as S
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P

type Element = Char
type Polymer = [Element]
type Mapping = Map (Element, Element) Element

-- Input

parser :: Parser (Polymer, Mapping)
parser = (,) <$> template <* P.eol <*> rules <* P.eof
  where
    template = P.some element <* P.eol
    rules = M.fromList <$> P.endBy1 rule P.eol
    rule = (\a b c -> ((a, b), c))
      <$> element <*> element <* P.chunk " -> " <*> element
    element = P.upperChar

input :: IO (Polymer, Mapping)
input = parseFile' parser "app/inputs/input14"

testInput :: IO (Polymer, Mapping)
testInput = parseFile' parser "app/inputs/input14_test"

-- Solvers

solve1 :: IO ()
solve1 = print . maxMinDiff . uncurry (flip $ countsAfter 10)
  -- =<< testInput
  =<< input

solve2 :: IO ()
solve2 = print . maxMinDiff . uncurry (flip $ countsAfter 40)
  -- =<< testInput
  =<< input

-- Logic

countsAfter :: Int -> Mapping -> Polymer -> Map Element Int
countsAfter n m p = collect . zipWith (f n) p $ tail p
  where
    collect = foldl' (M.unionWith (+)) $ M.singleton (head p) 1
    f  n x z = memo ! (n, x, z)
    memo = M.fromList [ ((i, x, z), f' i x z)
                      | i <- [1 .. n], x <- elements , z <- elements ]
    f' n x z
      | n == 1    = M.fromListWith (+) [(y, 1), (z, 1)]
      | otherwise = M.unionWith (+) (f (n-1) x y) (f (n-1) y z)
      where y = m ! (x, z)
    elements = S.toList . uncurry (<>) . (S.map fst &&& S.map snd) $ M.keysSet m

maxMinDiff :: (Foldable t, Num a, Ord a) => t a -> a
maxMinDiff = uncurry (-) . (maximum &&& minimum)
