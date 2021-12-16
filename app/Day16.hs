{-# LANGUAGE OverloadedStrings #-}

module Day16 (solve1, solve2) where

import Control.Monad        (when)
import Data.Bifunctor       (first, second)
import Data.Char            (digitToInt)
import Data.Foldable        (foldl')
import Data.Function        ((&))
import Data.Text            (Text)
import Text.Megaparsec
import Text.Megaparsec.Char

import Input (Parser, readFileUtf8', parse')

import qualified Data.Text as T

data Packet = Literal Version Value
            | Sum     Version [Packet]
            | Product Version [Packet]
            | Minimum Version [Packet]
            | Maximum Version [Packet]
            | Greater Version Packet Packet
            | Less    Version Packet Packet
            | Equal   Version Packet Packet

data LengthInfo = Bits    Int
                | Packets Int

type Version = Int
type Type    = Int
type Value   = Int

-- Input

input :: IO Packet
input = parse' parser f . toBitStream <$> readFileUtf8' f
  where f = "app/inputs/input16"

testInputs :: IO [Packet]
testInputs = pure $ map (parse' parser "" . toBitStream)
  -- Part 1 examples
  -- [ "D2FE28"
  -- , "38006F45291200"
  -- , "EE00D40C823060"
  -- , "8A004A801A8002F478"
  -- , "620080001611562C8802118E34"
  -- , "C0015000016115A2E0802F182340"
  -- , "A0016C880162017C3686B18A3D4780"

  -- Part 2 examples
  [ "C200B40A82"
  , "04005AC33890"
  , "880086C3E88112"
  , "CE00C43D881120"
  , "D8005AC2A8F0"
  , "F600BC2D8F"
  , "9C005AC2F8F0"
  , "9C0141080250320F1802104A08"
  ]

parser :: Parser Packet
parser = packet <* many (single '0') <* optional eol <* eof
  where
    packet  = (&) <$> version <*> (typeID >>= partialPacket)
    version = toInt <$> bits 3
    typeID  = toInt <$> bits 3

    bits :: Int -> Parser [Char]
    bits n = count n $ oneOf ['0', '1']

    partialPacket :: Type -> Parser (Value -> Packet)
    partialPacket 4 = flip Literal <$> literalValue
    partialPacket 0 = flip Sum     <$> (lengthInfo >>= subpackets)
    partialPacket 1 = flip Product <$> (lengthInfo >>= subpackets)
    partialPacket 2 = flip Minimum <$> (lengthInfo >>= subpackets)
    partialPacket 3 = flip Maximum <$> (lengthInfo >>= subpackets)
    partialPacket 5 = f    Greater <$  lengthInfo <*> packet <*> packet
    partialPacket 6 = f    Less    <$  lengthInfo <*> packet <*> packet
    partialPacket 7 = f    Equal   <$  lengthInfo <*> packet <*> packet
    f pack a b v = pack v a b

    literalValue :: Parser Value
    literalValue = (\gs g -> toInt $ foldr (++) g gs)
      <$> many (single '1' *> bits 4) <* single '0' <*> bits 4

    lengthInfo :: Parser LengthInfo
    lengthInfo = single '0' *> (Bits    . toInt <$> bits 15) <|>
                 single '1' *> (Packets . toInt <$> bits 11)
    
    subpackets :: LengthInfo -> Parser [Packet]
    subpackets (Bits    n) =
      packetsUntilAt . (+ n) . stateOffset =<< getParserState
    subpackets (Packets n) = count n packet

    packetsUntilAt :: Int -> Parser [Packet]
    packetsUntilAt i = do
      j <- stateOffset <$> getParserState
      when (i < j) $ fail "Went past bit count"
      if i == j
        then return []
        else (:) <$> packet <*> packetsUntilAt i

    toInt :: String -> Int
    toInt = foldl' (\num digit -> 2*num + digit) 0 . map digitToInt

toBitStream :: Text -> Text
toBitStream = T.concatMap f
  where
    f '0' = "0000"
    f '1' = "0001"
    f '2' = "0010"
    f '3' = "0011"
    f '4' = "0100"
    f '5' = "0101"
    f '6' = "0110"
    f '7' = "0111"
    f '8' = "1000"
    f '9' = "1001"
    f 'A' = "1010"
    f 'B' = "1011"
    f 'C' = "1100"
    f 'D' = "1101"
    f 'E' = "1110"
    f 'F' = "1111"
    f  x  = T.singleton x

-- Solvers

solve1 :: IO ()
-- solve1 = putStrLn "" >> testInputs >>= mapM_ (print . versionSum)
solve1 = print . versionSum =<< input
  where
    versionSum (Literal v _) = v
    versionSum (Sum     v ps) = v + (sum $ map versionSum ps)
    versionSum (Product v ps) = v + (sum $ map versionSum ps)
    versionSum (Minimum v ps) = v + (sum $ map versionSum ps)
    versionSum (Maximum v ps) = v + (sum $ map versionSum ps)
    versionSum (Greater v p1 p2) = v + versionSum p1 + versionSum p2
    versionSum (Less    v p1 p2) = v + versionSum p1 + versionSum p2
    versionSum (Equal   v p1 p2) = v + versionSum p1 + versionSum p2

solve2 :: IO ()
-- solve2 = mapM_ (print . eval) =<< (putStrLn " " >> testInputs)
solve2 = print . eval =<< input

-- Logic

eval :: Packet -> Value
eval (Literal _ x )                         = x
eval (Sum     _ ps)                         = sum     $ map eval ps
eval (Product _ ps)                         = product $ map eval ps
eval (Minimum _ ps)                         = minimum $ map eval ps
eval (Maximum _ ps)                         = maximum $ map eval ps
eval (Greater _ p1 p2) | eval p1 >  eval p2 = 1
                       | otherwise          = 0
eval (Less    _ p1 p2) | eval p1 <  eval p2 = 1
                       | otherwise          = 0
eval (Equal   _ p1 p2) | eval p1 == eval p2 = 1
                       | otherwise          = 0
