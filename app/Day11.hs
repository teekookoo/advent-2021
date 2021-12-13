{-# LANGUAGE TupleSections #-}

module Day11 (solve1, solve2) where

import Control.Arrow (second)
import Control.Monad (filterM)
import Control.Monad.ST (runST)
import Control.Monad.State.Lazy (State, evalState, state)
import Data.Char (digitToInt, intToDigit)
import Data.IntSet (IntSet)
import Data.List (findIndex)
import Data.Maybe (fromJust, mapMaybe)
import Data.Vector.Unboxed (Vector, MVector)

import Input (parseFile', Parser)

import qualified Data.IntSet as S
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P


data Grid = Grid Int (Vector Int)

-- Input

parser :: Parser Grid
parser = toGrid <$> P.endBy1 row P.eol <* P.eof
  where
    row = P.some (digitToInt <$> P.digitChar) 
    toGrid rs = Grid (length rs) $ V.fromList $ concat rs

testInput :: IO Grid
testInput = parseFile' parser "app/inputs/input11_test"

input :: IO Grid
input = parseFile' parser "app/inputs/input11"

-- Solvers

solve1 :: IO ()
solve1 = print . sum . take 100 . flashCounts
  -- =<< testInput
  =<< input

solve2 :: IO ()
solve2 = print . fromJust . fmap (+ 1) . findIndex (== 100) . flashCounts
  -- =<< testInput
  =<< input

-- Logic

flashCounts :: Grid -> [Int]
flashCounts = evalState . sequence . repeat $ state advance

advance :: Grid -> (Int, Grid)
advance (Grid n v) = second (Grid n) . doFlashes $ V.map (+ 1) v
  where
    doFlashes v = runST $ go S.empty (initialFlashes v) =<< V.thaw v

    initialFlashes = S.fromDistinctAscList
                   . V.ifoldr' (\i x is -> if x > 9 then i:is else is) []

    go flashed toFlash mv
      | S.null toFlash = (S.size flashed, ) <$> V.unsafeFreeze mv
      | otherwise      = do
          toFlash' <- S.foldl' (\s i -> s <> flash i) (return S.empty) toFlash
          go flashed' toFlash' mv
          where
            flashed' = flashed <> toFlash
            flash i = do
              let nbs = filter (`S.notMember` flashed') $ neighbors n i
              MV.write mv i 0
              mapM_ (MV.modify mv (+ 1)) nbs
              S.fromList <$> filterM (fmap (> 9) . MV.read mv) nbs

neighbors :: Int -> Int -> [Int]
neighbors n i = mapMaybe p2i nbCandidates
  where
    p2i (x, y)
      | 0 <= x && x < n && 0 <= y && y < n = Just $ y*n + x
      | otherwise                          = Nothing
    x = i `mod` n
    y = i `div` n
    nbCandidates = [ (x-1, y-1), (x, y-1), (x+1, y-1)
                   , (x-1, y  ),           (x+1, y  )
                   , (x-1, y+1), (x, y+1), (x+1, y+1) ]
    
pretty :: Grid -> String
pretty (Grid n v) = unlines [ V.toList $ V.slice i n s | i <- [0, n .. n*(n-1)] ]
  where s = V.map intToDigit v
