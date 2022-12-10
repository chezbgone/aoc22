{-# LANGUAGE OverloadedStrings #-}
module Day05 (solutions) where

import Control.Applicative
import Data.Maybe
import Data.List
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Void (Void)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

type Box = Char
type Stacks = Vector [Char]
type Pile = Int
type MoveInstruction = (Int, Pile, Pile)

-------------
-- Parsing --
-------------

type Parser = P.Parsec Void Text

parseBox :: Parser Box
parseBox = do
  _ <- P.char '['
  c <- P.anySingle
  _ <- P.char ']'
  pure c

parseCell :: Parser (Maybe Box)
parseCell =
  pure <$> parseBox <|>
    P.string "   " *> pure Nothing

parseStacks :: Parser Stacks
parseStacks = do
  cells <- (parseCell `P.sepBy` P.char ' ') `P.sepBy` P.newline
  pure $ V.fromList $ map (reverse . catMaybes) $ transpose $ reverse cells

parseInstruction :: Parser MoveInstruction
parseInstruction = do
  _    <- P.string "move "
  n    <- L.decimal
  _    <- P.string " from "
  from <- L.decimal
  _    <- P.string " to "
  to   <- L.decimal
  _    <- P.newline
  pure (n, from - 1, to - 1) -- subtract 1 for zero indexing

parser :: Parser (Stacks, [MoveInstruction])
parser = do
  stacks <- parseStacks
  _ <- P.manyTill P.anySingle P.newline
  _ <- P.manyTill P.anySingle P.newline
  instructions <- many parseInstruction
  pure (stacks, instructions)

parse :: Text -> (Stacks, [MoveInstruction])
parse = fromJust . P.parseMaybe parser

-------------
-- Solving --
-------------

shiftStacks :: Int -> Pile -> Pile -> Stacks -> Stacks
shiftStacks n from to stacks =
  let
    (moved, new_from) = splitAt n (stacks V.! from)
    old_to = stacks V.! to
  in
    stacks V.// [(from, new_from), (to, reverse moved ++ old_to)]

solveA :: (Stacks, [MoveInstruction]) -> String
solveA (stacks, instrs) =
  V.toList $ V.map head $ foldl' update_stack stacks instrs
    where update_stack st (n, from, to) = shiftStacks n from to st

liftStacks :: Int -> Pile -> Pile -> Stacks -> Stacks
liftStacks n from to stacks =
  let
    (moved, new_from) = splitAt n (stacks V.! from)
    old_to = stacks V.! to
  in
    stacks V.// [(from, new_from), (to, moved ++ old_to)]

solveB :: (Stacks, [MoveInstruction]) -> String
solveB (stacks, instrs) =
  V.toList $ V.map head $ foldl' update_stack stacks instrs
    where update_stack st (n, from, to) = liftStacks n from to st

solutions :: [Text -> String]
solutions = map (\s -> s . parse) [solveA, solveB]
