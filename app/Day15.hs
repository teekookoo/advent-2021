{-# LANGUAGE TupleSections #-}

module Day15 (solve1, solve2) where

import Control.Arrow (first)
import Control.Monad (when)
import Data.Char     (digitToInt)
import Data.Functor  (($>))
import Data.PSQueue  (PSQ, Binding (..))

import Input (Parser, parseFile')

import qualified Data.PSQueue                as Q
import qualified Data.Vector                 as B
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Text.Megaparsec             as P
import qualified Text.Megaparsec.Char        as P


type Vertex = Int
type Weight = Int
type Graph  = B.Vector (U.Vector (Vertex, Weight))

-- Input

testInput :: Int -> Int ->  IO Graph
testInput tx ty = parseFile' (parser tx ty) "app/inputs/input15_test"

input :: Int -> Int -> IO Graph
input tx ty = parseFile' (parser tx ty) "app/inputs/input15"

parser :: Int -> Int -> Parser Graph
parser tx ty = toGraph <$> rows <* P.eof
  where
    rows = tileY <$> P.endBy1 row P.eol
    row  = tileX <$> P.some (digitToInt <$> P.digitChar)

    tileX = concat . take tx . iterate f
    tileY = concat . take ty . iterate (map f)
    f = map ((+ 1) .  (`mod` 9))
    
    toGraph :: [[Int]] -> Graph
    toGraph rows =
      let v  = B.fromList $ concat rows
          nx = length $ head rows
       in B.imap (neighbors nx v) v

    neighbors :: Int -> B.Vector Int -> Int -> Int -> U.Vector (Vertex, Weight)
    neighbors nx v i _ = U.mapMaybe (\j -> (j,) <$> v B.!? j) $ indices nx i

    indices :: Int -> Int -> U.Vector Int
    indices nx i = let x = i `mod` nx
                       y = i `div` nx
                    in U.map (\(x, y) -> y*nx + x)
                     . U.filter (\(x,_) -> 0 <= x && x < nx)
                     $ U.fromList [ (x, y-1), (x-1, y), (x+1, y), (x, y+1) ]

-- Solvers

solve1 :: IO ()
solve1 = print . U.last . sssd 0
  -- =<< testInput 1 1
  =<< input 1 1

solve2 :: IO ()
solve2 = print . U.last . sssd 0
  -- =<< testInput 5 5
  =<< input 5 5

-- Logic

-- Single source shortest distance: uses Dijkstra with a priority search queue
sssd :: Vertex -> Graph -> U.Vector Int
sssd source graph = U.create $ do
  dist0 <- M.generate (B.length graph) gen
  go dist0 (Q.singleton source 0)
  where
    gen v
      | v == source = 0
      | otherwise   = maxBound

    go dist q
      | Q.null q  = return dist
      | otherwise = go dist =<< relaxAndQueue =<< M.read dist u
      where
        Just (u :-> _, rest) = Q.minView q
        neighbors            = graph B.! u
        relaxAndQueue du     = U.foldM' (f du) rest neighbors

        f du q' (v, duv) = do
          dv <- M.read dist v
          let dv' = du + duv
          if dv' < dv
            then M.write dist v dv' $> Q.insert v dv' q'
            else return q'
