{-# LANGUAGE OverloadedStrings #-}
module Day11 (solutions) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Data.Bifunctor (second)
import Data.List
import Data.Monoid
import Data.Maybe (fromJust)
import Data.Ord
import Data.Sequence (Seq)
import Data.Sequence qualified as S
import Data.Text (Text)
import Data.Vector(Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Void (Void)

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

type WorryLevel = Integer
type MonkeyIndex = Int
data Monkey = Monkey
  { _items :: Seq WorryLevel
  , _inspect :: WorryLevel -> WorryLevel
  , _modulus :: Integer
  , _throw :: WorryLevel -> MonkeyIndex
  }

instance Show Monkey where
  show m = show (_items m, _modulus m)

-------------
-- Parsing --
-------------

data Operand = OldValue | Literal Integer
operand :: Integer -> Operand -> Integer
operand oldValue value =
  case value of
    OldValue -> oldValue
    Literal n -> n

monkey_parser :: P.Parsec Void Text Monkey
monkey_parser = do
  _ <- P.string "Monkey "
    >> L.decimal @_ @_ @_ @Integer
    >> P.string ":"
    >> P.newline
    >> P.string "  Starting items: "
  starting_items <- S.fromList <$> L.decimal `P.sepBy` P.string ", "
  _ <- P.newline
    >> P.string "  Operation: new = "
  left <- (P.string "old" *> pure OldValue) P.<|> (Literal <$> L.decimal)
  _ <- P.space
  op <- P.string "+" *> pure (+) P.<|>
        P.string "*" *> pure (*)
  _ <- P.space
  right <- (P.string "old" *> pure OldValue) P.<|> (Literal <$> L.decimal)
  _ <- P.newline
  _ <- P.string "  Test: divisible by "
  modulus <- L.decimal
  _ <- P.newline
    >> P.string "    If true: throw to monkey "
  true_monkey <- L.decimal
  _ <- P.newline
    >> P.string "    If false: throw to monkey "
  false_monkey <- L.decimal
  _ <- P.newline
  let update_worry_level worry_level = op l r
        where
          l = operand worry_level left
          r = operand worry_level right
  let throw worry = if worry `mod` modulus == 0 then true_monkey else false_monkey
  pure $ Monkey starting_items update_worry_level modulus throw

parse :: Text -> Vector Monkey
parse = V.fromList . fromJust . P.parseMaybe (monkey_parser `P.sepBy` P.newline)

-------------
-- Solving --
-------------

type InteractionCount = Int

oneTurn :: Integer  -- calm factor
        -> MonkeyIndex
        -> Vector (InteractionCount, Monkey)
        -> Vector (InteractionCount, Monkey)
oneTurn calm_factor thrower monkeys = runST $ do
  let efficiency_modulus = foldl1 lcm $ V.map (_modulus . snd) monkeys
  monks_mut <- V.thaw monkeys
  (interactions, monk@(Monkey items inspect _ throw)) <- MV.read monks_mut thrower
  forM_ items $ \item -> do
    let new_item = (inspect item `div` calm_factor) `mod` efficiency_modulus
    let receiver = throw new_item
    let give_item :: WorryLevel -> Monkey -> Monkey
        give_item x addee = addee {_items = _items addee S.|> x}
    MV.modify monks_mut (second (give_item new_item)) receiver
  MV.write
    monks_mut
    thrower
    (interactions + S.length items, monk{_items=S.empty})
  V.freeze monks_mut

oneRound :: Integer  -- calm factor
         -> Vector (InteractionCount, Monkey)
         -> Vector (InteractionCount, Monkey)
oneRound calm_factor monkeys =
  flip execState monkeys $
    V.iforM_ monkeys $
      \i _ -> modify $ oneTurn calm_factor i

nRounds :: Integer -> Int
        -> Vector (InteractionCount, Monkey) -> Vector (InteractionCount, Monkey)
nRounds calm_factor n =
  appEndo $ mconcat $ replicate n $ Endo (oneRound calm_factor)

solveA :: Vector Monkey -> Int
solveA =
  product
  . take 2
  . sortOn Down
  . map fst
  . V.toList
  . nRounds 3 20
  . V.map (0,)

solveB :: Vector Monkey -> Int
solveB =
  product
  . take 2
  . sortOn Down
  . map fst
  . V.toList
  . nRounds 1 10000
  . V.map (0,)

solutions :: [Text -> String]
solutions = map (\s -> show . s . parse) [solveA, solveB]
