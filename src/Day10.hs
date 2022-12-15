{-# LANGUAGE OverloadedStrings #-}
module Day10 (solutions) where

import Data.Maybe
import Data.List.Split
import Data.Text (Text)
import Data.Void

import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

data Instruction = Noop | Addx Int
  deriving Show
type Signal = Int

-------------
-- Parsing --
-------------

type Parser = Parsec Void Text

addx_parser :: Parser Instruction
addx_parser = do
  _ <- P.string "addx "
  n <- L.signed P.space L.decimal
  _ <- P.newline
  pure $ Addx n

noop_parser :: Parser Instruction
noop_parser = do
  _ <- P.string "noop"
  _ <- P.newline
  pure Noop

parser :: Parser [Instruction]
parser = P.many (addx_parser P.<|> noop_parser)

parse :: Text -> [Instruction]
parse = fromJust . P.parseMaybe parser

-------------
-- Solving --
-------------

compute_signals :: [Instruction] -> [Signal]
compute_signals = reverse . signals' [1]
  where
    signals' :: [Signal] -> [Instruction] -> [Signal]
    signals' ss [] = ss
    signals' [] _ = error "no initial signal"
    signals' sigs@(s:_) (i:is) =
      case i of
        Addx n -> signals' ((s+n):s:sigs) is
        Noop -> signals' (s:sigs) is

solveA :: [Instruction] -> String
solveA is = show $ sum $ do
  i <- [20, 60, 100, 140, 180, 220]
  let signal = signals !! (i - 1)
  pure $ i * signal
  where
    signals = compute_signals is

solveB :: [Instruction] -> String
solveB is = unlines $ map drawLine signals
  where
    signals = chunksOf 40 $ compute_signals is
    drawLine :: [Signal] -> String
    drawLine = map pixel . zip [0..]
    pixel :: (Int, Signal) -> Char
    pixel (i, s) = if abs (i - s) <= 1 then '#' else ' '

solutions :: [Text -> String]
solutions = map (\s -> s . parse) [solveA, solveB]
