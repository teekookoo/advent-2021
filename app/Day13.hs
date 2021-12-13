{-# LANGUAGE OverloadedStrings #-}

module Day13 (solve1, solve2) where

import Control.Applicative ((<|>))
import Control.Arrow       ((&&&), first, second)
import Data.Function       ((&))
import Data.List           (foldl')
import Data.Set            (Set)

import Input (parseFile', Parser)

import qualified Data.Set             as S
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P

type Dots = Set (Int, Int)
type Fold = Dots -> Dots

-- Input

input :: IO (Dots, [Fold])
input = parseFile' parser "app/inputs/input13"

testInput :: IO (Dots, [Fold])
testInput = parseFile' parser "app/inputs/input13_test"

parser :: Parser (Dots, [Fold])
parser = (,) <$> dots <* P.eol <*> folds <* P.eof
  where
    dots = S.fromList <$> dot `P.endBy1` P.eol
    dot = (,) <$> int <* P.single ',' <*> int

    folds = fold `P.endBy1` P.eol
    fold = P.chunk "fold along "
       *> (mkFold first <$ P.chunk "x=" <|> mkFold second <$ P.chunk "y=")
      <*> int

    int = read <$> P.some P.digitChar

-- Solvers

solve1 :: IO ()
-- solve1 = print . S.size . uncurry (&) . second head =<< testInput
solve1 = print . S.size . uncurry (&) . second head =<< input

solve2 :: IO ()
-- solve2 = putStrLn . pretty . uncurry (foldl' (&)) =<< testInput
solve2 = putStrLn . pretty . uncurry (foldl' (&)) =<< input

-- Logic

mkFold :: ((Int -> Int) -> (Int, Int) -> (Int, Int)) -> Int -> Fold
mkFold f ax = S.map (f (\t -> if t <= ax then t else 2*ax - t))

pretty :: Dots -> String
pretty ds = unlines [ [ if (x, y) `S.member` ds then '▮' else ' '
                      | x <- [xMin .. xMax] ]
                    | y <- [yMin .. yMax] ]
  where
    (xMin, xMax) = (minimum &&& maximum) $ S.map fst ds
    (yMin, yMax) = (minimum &&& maximum) $ S.map snd ds
