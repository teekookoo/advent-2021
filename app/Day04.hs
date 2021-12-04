{-# LANGUAGE TupleSections #-}

module Day04 (solve1, solve2) where

import           Control.Arrow ((&&&), second)
import           Data.Function ((&))
import           Data.IntMap (IntMap)
import qualified Data.IntMap as M
import           Data.List (find, foldl', scanl')
import           Data.Maybe (listToMaybe, mapMaybe)
import           Data.Tuple (swap)
import           Text.Parsec

data BingoGrid = BingoGrid
                 (IntMap Int)          -- Mapping from bingo number to grid index
                 (IntMap (Int, Bool))  -- Mapping from grid index to bingo number
                                       -- and a flag indicating whether the
                                       -- number has been selected

-- Input handling

testInput :: IO ([Int], [BingoGrid])
testInput = parseInput <$> readFile "app/inputs/input04_test"
   
input :: IO ([Int], [BingoGrid])
input = parseInput <$> readFile "app/inputs/input04"

parseInput :: String -> ([Int], [BingoGrid])
parseInput s = case parse bingoData "" s of Left  err -> error $ show err
                                            Right res -> res
  where
    bingoData = (,) <$> bingoNumbers <* (count 2 endOfLine) <*> bingoGrids <* eof
    bingoNumbers = int `sepBy1` char ','
    bingoGrids = bingoGrid `sepBy1` endOfLine
    bingoGrid = initialize <$> count 5 bingoRow
    bingoRow = count 5 (spaces *> int) <* endOfLine
    int = read <$> many1 digit

-- Solvers

solve1 :: IO ()
solve1 = do
  -- (nums, grids) <- testInput
  (nums, grids) <- input
  let firstWinner = listToMaybe
                  . mapMaybe (\(n, gs) -> (n,) <$> find hasWon gs)
                  . zip nums
                  . drop 1
                  $ rounds grids nums
  case firstWinner of
    Nothing -> putStrLn "Error: no board wins???"
    Just (num, grid) -> print $ score num grid

solve2 :: IO ()
solve2 = do
  -- (nums, grids) <- testInput
  (nums, grids) <- input
  let lastWinner = (\(n, gs) -> (n,) <$> find hasWon gs)
                 . last
                 . zip nums
                 . drop 1
                 $ rounds grids nums
  case lastWinner of
    Nothing -> putStrLn "Error: no board wins???"
    Just (num, grid) -> print $ score num grid

-- Logic

initialize :: [[Int]] -> BingoGrid
initialize = uncurry BingoGrid . (f n2i &&& f i2n) . zip [0..] . concat
  where
    f g = M.fromList . map g
    n2i = swap
    i2n = second (, False)

addNumber :: Int -> BingoGrid -> BingoGrid
addNumber n (BingoGrid n2i i2n) =
  case M.lookup n n2i of
    Nothing -> BingoGrid n2i i2n
    Just i  -> BingoGrid n2i $ M.insert i (n, True) i2n

hasWon :: BingoGrid -> Bool
hasWon (BingoGrid _ i2n) = any and . bingoLines $ M.map snd i2n
  where
    bingoLines m = map (map (m M.!)) $ rowIdx ++ colIdx
    rowIdx = [ [r .. r+4] | r <- [0, 5 .. 20] ]
    colIdx = [ [c, c+5 .. c+20] | c <- [0 .. 4] ]

score :: Int -> BingoGrid -> Int
score num (BingoGrid _ i2n) = num * M.foldl' f 0 i2n
  where
    f x (n, False) = x + n
    f x _          = x

rounds :: [BingoGrid] -> [Int] -> [[BingoGrid]]
rounds [] _ = []
rounds gs (n:ns) = gs : rounds (map (addNumber n) $ filter (not . hasWon) gs) ns
