module Day18 (solve1, solve2) where

import Control.Monad        ((<=<))
import Data.Bifunctor       (first, second)
import Data.Char            (digitToInt)
import Data.List            (foldl1')
import Data.Maybe           (fromJust)
import Text.Megaparsec
import Text.Megaparsec.Char

import Input (Parser, parseFile')

data Root = Root Tree Tree

data Tree = Tree Tree Tree -- Tree Left Right
          | Leaf Int       -- Leaf Int

data Direction = L | R

data Walk = Walk Tree Int [(Tree, Direction)]

-- Input

parser :: Parser [Root]
parser = endBy1 root eol <* eof
  where
    root = uncurry Root <$> pair'
    pair = uncurry Tree <$> pair'
    pair' = (,) <$ single '[' <*> element <* single ',' <*> element <* single ']'
    element = pair <|> (Leaf . digitToInt <$> digitChar)

testInput :: IO [Root]
testInput = parseFile' parser "app/inputs/input18_test"

input :: IO [Root]
input = parseFile' parser "app/inputs/input18"

-- Solvers

solve1 :: IO ()
-- solve1 = print . magnitude . foldl1 add =<< testInput
solve1 = print . magnitude . foldl1' add =<< input

solve2 :: IO ()
-- solve2 = print . maximum . map magnitude . sums =<< testInput
solve2 = print . maximum . map magnitude . sums =<< input
  where
    sums nums = do
      (a, k) <- zip nums [0 ..]
      b <- uncurry (++) . second (drop 1) $ splitAt k nums
      return $ add a b

-- Logic

initWalk :: Root -> Walk
initWalk (Root l r) = Walk (Tree l r) 0 []

endWalk :: Walk -> Maybe Root
endWalk w = case top w of Walk (Tree l r) _ _ -> Just $ Root l r
                          _                   -> Nothing

top :: Walk -> Walk
top w@(Walk _ _ []) = w
top w               = top $ up w

up :: Walk -> Walk
up (Walk t d ((r, L) : path)) = Walk (Tree t r) (d-1) path
up (Walk t d ((l, R) : path)) = Walk (Tree l t) (d-1) path

left :: Walk -> Walk
left (Walk (Tree l r) d path) = Walk l (d+1) $ (r, L) : path

right :: Walk -> Walk
right (Walk (Tree l r) d path) = Walk r (d+1) $ (l, R) : path

farLeft :: Walk -> Walk
farLeft w@(Walk (Tree _ _) _ _) = farLeft $ left w
farLeft w                       = w

farRight :: Walk -> Walk
farRight w@(Walk (Tree _ _) _ _) = farRight $ right w
farRight w                       = w

toLeafLeft :: Walk -> Either Walk Walk
toLeafLeft = fmap (farRight . left) . upToR
  where
    upToR w@(Walk _ _ path) = case path of
      (_, L) : _ -> upToR $ up w
      (_, R) : _ -> Right $ up w
      []         -> Left w

toLeafRight :: Walk -> Either Walk Walk
toLeafRight = fmap (farLeft . right) . upToL
  where
    upToL w@(Walk _ _ path) = case path of
      (_, R) : _ -> upToL $ up w
      (_, L) : _ -> Right $ up w
      []         -> Left w

add :: Root -> Root -> Root
add (Root l r) (Root l' r') =
  fromJust $ endWalk $ reduce $ initWalk $ (l `Tree` r) `Root` (l' `Tree` r')
  where
    reduce = either reduce id . (split <=< explode)

-- Left: one pair has been exploded
-- Right: no pairs to explode
explode :: Walk -> Either Walk Walk
explode = first commit . toExploding . farLeft
  where
    toExploding w@(Walk _ d (((Leaf _), _) : _))
      | d >= 5 = Left $ up w
      | otherwise = tryRight w
    toExploding w = tryRight w

    tryRight w = case toLeafRight w of Left w  -> Right w
                                       Right w -> toExploding w

    commit (Walk (Tree l r) d path@((_, L) : _)) =
      either id top $
      toLeafRight (Walk (Leaf 0) d path) >>=
      toLeafLeft . addLeaf r >>=
      second (addLeaf l) . toLeafLeft

    commit (Walk (Tree l r) d path@((_, R) : _)) =
      either id top $
      toLeafLeft (Walk (Leaf 0) d path) >>=
      toLeafRight . addLeaf l >>=
      second (addLeaf r) . toLeafRight
    
    addLeaf :: Tree -> Walk -> Walk
    addLeaf (Leaf n) (Walk (Leaf m) d path) = Walk (Leaf $ m+n) d path

-- Left: one number has been split
-- Right: no numbers to split
split :: Walk -> Either Walk Walk
split = first commit . toSplitting . farLeft
  where
    toSplitting w@(Walk (Leaf n) _ _)
      | n >= 10   = Left w
      | otherwise = either Right toSplitting $ toLeafRight w

    commit (Walk (Leaf n) d path) = top $ Walk (split n) d path

    split n = Leaf (n `div` 2) `Tree` Leaf ((n+1) `div` 2)

pretty :: Root -> String
pretty (Root a b) = go $ Tree a b
  where
    go (Leaf n) = show n
    go (Tree a b) = "[" ++ go a ++ "," ++ go b ++ "]"

magnitude :: Root -> Int
magnitude (Root l r) = go $ Tree l r
  where
    go (Tree l r) = 3 * go l + 2 * go r
    go (Leaf n)   = n
