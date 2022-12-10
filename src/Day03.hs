{-# LANGUAGE OverloadedStrings #-}
module Day03 (solutions) where

import Data.Char
import Data.List
import Data.List.Split (chunksOf)

split :: String -> ([Char], [Char])
split xs = splitAt (length xs `div` 2) xs

parse :: String -> [[Char]]
parse t = lines t

priority :: Char -> Int
priority c
  | 'a' <= c && c <= 'z' = 1 + ord c - ord 'a'
  | 'A' <= c && c <= 'Z' = 27 + ord c - ord 'A'
  | otherwise = error "should not happen"

solveA :: [[Char]] -> Int
solveA xs = sum $ map (priority . head . uncurry intersect . split) xs

solveB :: [String] -> Int
solveB xs = sum $ map (priority . head . foldl1' intersect) $ chunksOf 3 xs

solutions :: [String -> String]
solutions = map (\s -> show . s . parse) [solveA, solveB]
