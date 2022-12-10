{-# LANGUAGE OverloadedStrings #-}
module Day06 (solutions) where

import Data.List
import Data.Maybe

parse :: String -> String
parse = id

solve :: Int -> String -> Int
solve n xs = (n +) $ fromJust $ findIndex (allDistinct . take n) $ tails xs
  where allDistinct ys = length ys == length (nub ys)

solveA :: String -> Int
solveA = solve 4

solveB :: String -> Int
solveB = solve 14

solutions :: [String -> String]
solutions = map (\s -> show . s . parse) [solveA, solveB]
