{-# LANGUAGE OverloadedStrings #-}
module Day04 (solutions) where

import Data.Maybe
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Parser = P.Parsec Void Text

type Range = (Int, Int)

parser :: Parser [(Range, Range)]
parser = P.many $ do
  a <- P.decimal
  _ <- P.char '-'
  b <- P.decimal
  _ <- P.char ','
  c <- P.decimal
  _ <- P.char '-'
  d <- P.decimal
  _ <- P.newline
  pure ((a, b), (c, d))

parse :: Text -> [(Range, Range)]
parse = fromJust . P.parseMaybe parser

contains :: Range -> Range -> Bool
(a, b) `contains` (c, d) = a <= c && d <= b

solveA :: [(Range, Range)] -> Int
solveA ranges = length
  [ ()
  | (r1, r2) <- ranges
  , r1 `contains` r2 || r2 `contains` r1
  ]

overlaps :: Range -> Range -> Bool
(a, b) `overlaps` (c, d) =
  a <= d && c <= b

solveB :: [(Range, Range)] -> Int
solveB ranges = length
  [ ()
  | (r1, r2) <- ranges
  , r1 `overlaps` r2
  ]

solutions :: [Text -> String]
solutions = map (\s -> show . s . parse) [solveA, solveB]
