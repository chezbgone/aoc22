{-# LANGUAGE OverloadedStrings #-}
module Day01 (solutions) where

import Data.List
import Data.Ord
import Data.Text (Text)
import Data.Text qualified as T

parse :: Text -> [[Int]]
parse t = map (read @Int . T.unpack) . T.words <$> T.splitOn "\n\n" t

solveA :: [[Int]] -> Int
solveA = maximum . map sum

solveB :: [[Int]] -> Int
solveB = sum . take 3 . sortOn Down . map sum

showOutput :: Int -> String
showOutput = show

solutions :: [Text -> String]
solutions = map (\s -> showOutput . s . parse) [solveA, solveB]
