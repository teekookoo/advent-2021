{-# LANGUAGE OverloadedLists #-}

module Day10 (solve1, solve2) where

-- Imports

import Data.Vector          (Vector, (!))
import Text.Megaparsec
import Text.Megaparsec.Char

import Input (Parser, parseFile')

import qualified Data.Vector                  as V
import qualified Data.Vector.Algorithms.Intro as V (select)

-- Data declarations

data Chunk = Legal      Bracket (Vector Chunk)
           | Corrupted  Bracket (Vector Chunk) Bracket
           | Incomplete Bracket (Vector Chunk)

data NonCorrupted = Legal'      Bracket (Vector NonCorrupted)
                  | Incomplete' Bracket (Vector NonCorrupted)

data ChunkPrefix = ChunkPrefix Bracket (Vector Chunk)

data Bracket = Round | Square | Curly | Angle
             deriving (Eq)

type Line = Vector Chunk

-- Input handling

testInput :: IO (Vector Line)
testInput = parseFile' parser "app/inputs/input10_test"

input :: IO (Vector Line)
input = parseFile' parser "app/inputs/input10"

parser :: Parser (Vector Line)
parser = V.fromList <$> endBy1 line eol <* eof
  where
    line = V.fromList <$> some chunk

    chunk = complete <$> prefix <*> optional closingBracket

    complete (ChunkPrefix b  cs)  Nothing              = Incomplete b  cs
    complete (ChunkPrefix b1 cs) (Just b2) | b1 == b2  = Legal      b1 cs
                                           | otherwise = Corrupted  b1 cs b2

    prefix = ChunkPrefix <$> openingBracket <*> (V.fromList <$> many chunk)

    openingBracket = ro <|> so <|> co <|> ao
    closingBracket = rc <|> sc <|> cc <|> ac

    ro = Round  <$ char '('
    rc = Round  <$ char ')'
    so = Square <$ char '['
    sc = Square <$ char ']'
    co = Curly  <$ char '{'
    cc = Curly  <$ char '}'
    ao = Angle  <$ char '<'
    ac = Angle  <$ char '>'

-- Solvers

solve1 :: IO ()
solve1 = print . score1 . V.mapMaybe firstIllegalBracket
  =<< input
  -- =<< testInput

solve2 :: IO ()
solve2 = print . median . V.map (score2 . repair) . V.mapMaybe toNonCorrupted
  =<< input
  -- =<< testInput

-- Logic

firstIllegalBracket :: Vector Chunk -> Maybe Bracket
firstIllegalBracket = V.foldr f Nothing
  where
    f chunk acc = case go chunk of Nothing -> acc
                                   b       -> b
    go (Legal      _ cs  ) = firstIllegalBracket cs
    go (Corrupted  _ cs b) = firstIllegalBracket cs <|> Just b
    go (Incomplete _ cs  ) = firstIllegalBracket cs

score1 :: Vector Bracket -> Int
score1 = V.sum . V.map points
  where
    points Round  = 3
    points Square = 57
    points Curly  = 1197
    points Angle  = 25137

toNonCorrupted :: Vector Chunk -> Maybe (Vector NonCorrupted)
toNonCorrupted = V.mapM f
  where
    f (Corrupted  _ _  _) = Nothing
    f (Legal      b cs  ) = Just . Legal'      b =<< toNonCorrupted cs
    f (Incomplete b cs  ) = Just . Incomplete' b =<< toNonCorrupted cs

repair :: Vector (NonCorrupted) -> Vector Bracket
repair = V.fromList . f [] . V.last
  where
    f suffix (Legal'      _ _  ) = suffix
    f suffix (Incomplete' b [] ) = b : suffix
    f suffix (Incomplete' b ncs) = f (b : suffix) $ V.last ncs

score2 :: Vector Bracket -> Int
score2 = V.foldl' (\total c -> 5*total + points c) 0
  where
    points Round  = 1
    points Square = 2
    points Curly  = 3
    points Angle  = 4

-- Assumes odd length
median :: Ord a => Vector a -> a
median v = V.minimum . V.drop m . V.modify (\mv -> V.select mv m) $ v
  where m = V.length v `div` 2
