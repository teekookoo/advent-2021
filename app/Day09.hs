module Day09 (solve1, solve2) where

import           Control.Arrow ((&&&))
import           Control.Monad ((<=<))
import           Data.Char (digitToInt)
import           Data.IntSet (IntSet)
import qualified Data.IntSet as S
import           Data.Maybe (mapMaybe)
import           Data.Ord (comparing)
import           Data.Vector.Unboxed (Vector, (!), (!?))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Algorithms.Intro as V

data Heightmap = HM (Vector Height) Int -- Numbers nx
type Point = (Int, Int)
type Basin = IntSet
type Height = Int
type Index = Int

testInput :: IO Heightmap
testInput = pure $ parseInput s
  where s = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678\n"

input :: IO Heightmap
input = parseInput <$> readFile "app/inputs/input09"

parseInput :: String -> Heightmap
parseInput s = HM (V.fromList $ concat nums) nx
  where
    nums = map (map digitToInt) $ lines s
    nx = length $ head nums

solve1 :: IO ()
solve1 = do
  -- hm <- testInput
  hm@(HM v _) <- input
  print . V.sum . V.mapMaybe (fmap (1 +) .  get hm) $ lowPoints hm

solve2 :: IO ()
solve2 = do
  -- hm <- testInput
  hm <- input
  print . V.product . greatest 3 . V.map (basinSize hm) $ lowPoints hm
  where
    greatest n = V.take n . V.modify (\v -> V.selectBy (comparing negate) v n)
    basinSize hm = S.size . basin hm

-- Logic

get :: Heightmap -> Point -> Maybe Height
get hm@(HM v _) = (v !?) . p2i hm <=< validate hm

validate :: Heightmap -> Point -> Maybe Point
validate (HM _ nx) (x, y)
  | 0 <= x && x < nx = Just (x, y)
  | otherwise        = Nothing

neighborPoints :: Point -> [Point]
neighborPoints (x, y) = [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]

neighbors :: Heightmap -> Point -> [Height]
neighbors hm = mapMaybe (get hm) . neighborPoints

lowPoints :: Heightmap -> Vector Point
lowPoints hm@(HM v nx) = V.imapMaybe lowPoint v
  where
    lowPoint i x
      | all (x <) . neighbors hm $ i2p hm i = Just $ i2p hm i
      | otherwise                           = Nothing


i2p :: Heightmap -> Index -> Point
i2p (HM _ nx) = (`mod` nx) &&& (`div` nx)

p2i :: Heightmap -> Point -> Index
p2i (HM _ nx) (x, y) = y*nx + x

basin :: Heightmap -> Point -> IntSet
basin hm p = go [p] S.empty S.empty
  where
    go [] _ b = b
    go (p:ps) s b
      | seen      = go ps  s  b
      | inBasin   = go ps' s' b'
      | otherwise = go ps  s' b
      where
        seen = i `S.member` s
        inBasin = case get hm p of Nothing -> False
                                   Just 9  -> False
                                   _       -> True
        ps' = neighborPoints p ++ ps
        s'  = S.insert i s
        b'  = S.insert i b
        i   = p2i hm p
        
