module Main where

import Data.String
import Data.Text (Text)
import Data.Text qualified as T

-- import Day01 qualified
-- import Day02 qualified
-- import Day03 qualified
-- import Day04 qualified
-- import Day05 qualified
-- import Day06 qualified
import Day07 qualified
-- import Day08 qualified
-- import Day09 qualified
-- import Day10 qualified
-- import Day11 qualified
-- import Day12 qualified

class IsString a => StringLike a where
  toString :: a -> String

instance StringLike String where
  toString = id

instance StringLike Text where
  toString = T.unpack

solve :: (StringLike a, StringLike b) => String -> [a -> b] -> IO ()
solve file solutions = do
  contents <- fromString <$> readFile file
  mapM_ putStrLn [toString (sol contents) | sol <- solutions]

main :: IO ()
main = do
  -- solve "inputs/day01" Day01.solutions
  -- solve "inputs/day02" Day02.solutions
  -- solve "inputs/day03" Day03.solutions
  -- solve "inputs/day04" Day04.solutions
  -- solve "inputs/day05" Day05.solutions
  -- solve "inputs/day06" Day06.solutions
  solve "inputs/day07" Day07.solutions
  -- solve "inputs/day08" Day08.solutions
  -- solve "inputs/day09" Day09.solutions
  -- solve "inputs/day10" Day10.solutions
