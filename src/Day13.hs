{-# LANGUAGE OverloadedStrings #-}
module Day13 (solutions) where

import Control.Monad
import Data.Maybe
import Data.List
import Data.Text (Text)
import Data.Void
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

data Packet = Value Int | List [Packet]
  deriving (Eq, Show)

instance Ord Packet where
  compare (Value l) (Value r) = compare l r
  compare (List ls) (List rs) = compare ls rs
  compare l@(Value _) rs = compare (List [l]) rs
  compare ls r@(Value _) = compare ls (List [r])

type Parser = Parsec Void Text

packetParser :: Parser Packet
packetParser = (Value <$> L.decimal) P.<|> do
  _ <- P.string "["
  subpackets <- packetParser `P.sepBy` P.string ","
  _ <- P.string "]"
  pure $ List subpackets

groupParser :: Parser (Packet, Packet)
groupParser = (,) <$> (packetParser <* P.newline) <*> (packetParser <* P.newline)

parser :: Parser [(Packet, Packet)]
parser = groupParser `P.sepBy1` P.newline

parse :: Text -> [(Packet, Packet)]
parse = fromJust . P.parseMaybe parser

solveA :: [(Packet, Packet)] -> Int
solveA pairs = sum $ do
  (i, (l, r)) <- zip [1..] pairs
  guard $ compare l r == LT
  pure i

solveB :: [(Packet, Packet)] -> Int
solveB packets = fromJust $ do
  i1 <- elemIndex key1 packets_sorted
  i2 <- elemIndex key2 packets_sorted
  pure $ (i1 + 1) * (i2 + 1)
  where
    all_packets = packets >>= \(a, b) -> [a, b]
    key1 = List [List [Value 2]]
    key2 = List [List [Value 6]]
    packets_sorted = sort $ key1 : key2 : all_packets


solutions :: [Text -> String]
solutions = map (\s -> show . s . parse) [solveA, solveB]
