{-# LANGUAGE OverloadedLists #-}

module Day08 (solve1, solve2) where

import           Data.Char (ord)
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Text.Parsec

type Signal = IntSet
type Input = Set Signal
type Output = Vector Signal
type Mapping = Map (Signal) Int

-- Input handling

testInput :: IO [(Input, Output)]
testInput = parseInput <$> readFile "app/inputs/input08_test"

input :: IO [(Input, Output)]
input = parseInput <$> readFile "app/inputs/input08"

parseInput :: String -> [(Input, Output)]
parseInput = either (error . show) id . parse notes ""
  where
    notes = (line `endBy1` endOfLine) <* eof
    line = (,) <$> signalSet <* string " | " <*> signalVector
    signalSet = S.fromList <$> signals
    signalVector = V.fromList <$> signals
    signals = signal `sepBy1` try (char ' ' <* notFollowedBy (char '|'))
    signal = fmap (IS.fromList . map ord) $ many1 $ oneOf "abcdefg"


-- Solvers

solve1 :: IO ()
-- solve1 = print . length . filter is1478 . concatMap snd =<< testInput
solve1 = print . sum . map (V.length . V.filter is1478 . snd) =<< input
  where
    is1478 s = IS.size s `notElem` ([5, 6] :: [Int])

solve2 :: IO ()
-- solve2 = print . sum . map f =<< testInput
solve2 = print . sum . map f =<< input
  where
    f (i, o) = either error id $ mapping i >>= \m -> eval m o

-- Logic

eval :: Mapping -> Output -> Either String Int
eval m = V.foldl' f (Right 0)
  where
    f (Right n) s = case M.lookup s m of
      Just d  -> Right $ d + 10 * n
      Nothing -> Left  $ "Key " ++ show s ++ " not in " ++ show m
    shiftl n = 10 * n

mapping :: Input -> Either String Mapping
mapping signals = case maybeMapping of Just m -> Right m
                                       Nothing -> Left "Failure in mapping"
  where
    maybeMapping = do
      -- Pick digits recognizable by number of segments
      (s1, rest) <- pickBySize 2 signals
      (s4, rest) <- pickBySize 4 rest
      (s7, rest) <- pickBySize 3 rest
      (s8, rest) <- pickBySize 7 rest

      -- Split into sets of 5- and 6-segment digits
      let (s069, s235) = S.partition (szEq 6) rest

      -- Deduce rest of the digits by intersecting with known digits
      (s3, s25 ) <- pickByIntersectionSize 2 s1 s235
      (s5, [s2]) <- pickByIntersectionSize 3 s4 s25
      (s6, s09 ) <- pickByIntersectionSize 1 s1 s069
      (s0, [s9]) <- pickByIntersectionSize 3 s4 s09

      -- Construct a mapping form signal to digit
      return . M.fromList $ zip [s0, s1, s2, s3, s4, s5, s6, s7, s8, s9] [0 ..]
      
    -- Helper functions
    pickBySize             n   = pickBy (szEq n)
    pickByIntersectionSize n s = pickBy (szEq n . IS.intersection s)
    szEq                   n   = (n ==) . IS.size

pickBy :: Ord a => (a -> Bool) -> Set a -> Maybe (a, Set a)
pickBy p s = (\e -> (e, S.delete e s)) <$> foldr f Nothing s
  where f x y | p x       = Just x
              | otherwise = y
