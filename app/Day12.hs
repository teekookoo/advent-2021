{-# LANGUAGE OverloadedStrings #-}

module Day12 (solve1, solve2) where

import Control.Applicative ((<|>), liftA2)
import Control.Arrow       ((&&&))
import Data.Map            (Map)
import Data.Maybe          (fromJust)
import Data.Text           (Text)

import Input (Parser, parseFile')

import qualified Data.Map             as M
import qualified Data.Set             as S
import qualified Data.Text            as T
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P

data Vertex = Big   Text
            | Small Text
            deriving (Eq, Ord)

type Graph = Map Vertex [Vertex]

-- Input

testInput :: IO Graph
-- testInput = parseFile' parser "app/inputs/input12_test_1"
-- testInput = parseFile' parser "app/inputs/input12_test_2"
testInput = parseFile' parser "app/inputs/input12_test_3"

input :: IO Graph
input = parseFile' parser "app/inputs/input12"

parser :: Parser Graph
parser = toMap <$> P.endBy1 edge P.eol <* P.eof
  where
    edge = (,) <$> vertex <* P.single '-' <*> vertex
    vertex = (Big . T.pack) <$> P.some P.upperChar
       <|> (Small . T.pack) <$> P.some P.lowerChar
    toMap = M.fromListWith (<>)
          . concatMap (\(a, b) -> [(a, [b]), (b, [a])])

-- Solvers

solve1 :: IO ()
solve1 = print . fromJust . numPaths1 startVertex endVertex =<< testInput
-- solve1 = print . numPaths1 startVertex endVertex =<< input

solve2 :: IO ()
-- solve2 = print . numPaths2 startVertex endVertex =<< testInput
solve2 = print . fromJust . numPaths2 startVertex endVertex =<< input

-- Logic

startVertex :: Vertex
startVertex = Small "start"

endVertex :: Vertex
endVertex = Small "end"

-- Compute number of paths with a DFS
numPaths1 :: Vertex -> Vertex -> Graph -> Maybe Int
numPaths1 from to graph = go S.empty from
  where
    go seen v
      | v == to = Just 1
      | otherwise = fmap sum . mapM (go seen') . filter (`S.notMember` seen) =<< M.lookup v graph
      where
        seen' = case v of
          (Big _)   -> seen
          (Small _) -> S.insert v seen

-- Compute number of paths by generating all possibilities with
-- two layers of DFS and then thinning it down to unique paths with a Set.
-- Could be prettier but this is my third completed day today and it works
-- TODO optimize
numPaths2 :: Vertex -> Vertex -> Graph -> Maybe Int
numPaths2 from to graph = (S.size . S.fromList) <$> go1 S.empty from
  where
    go1 seen v@(Big _)
      | v == to = Just [[v]]
      | v == from = prepend v . mapM (go1 $ add v seen) =<< nbs v seen
      | otherwise = prepend v . mapM (go1 seen)         =<< nbs v seen

    go1 seen v
      | v == to = Just [[v]]
      | v == from = prepend v . mapM (go1 $ add v seen)           =<< nbs v seen
      | otherwise = prepend v . uncurry (liftA2 (++)) . (f &&& g) =<< nbs v seen
      where
        f = mapM (go1 $ add v seen)
        g = mapM (go2 seen)

    go2 seen v
      | v == to = Just [[v]]
      | otherwise = prepend v . mapM (go2 seen') =<< nbs v seen
      where
        seen' = case v of
          (Big _)   -> seen
          (Small _) -> S.insert v seen

    add = S.insert
    prepend v = fmap (map (v:) . concat)
    nbs v seen = filter (`S.notMember` seen) <$> M.lookup v graph
