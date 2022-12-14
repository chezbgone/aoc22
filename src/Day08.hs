{-# LANGUAGE OverloadedStrings #-}
module Day08 (solutions) where

import Data.Char (ord)
import Data.List (transpose)

charToInt :: Char -> Int
charToInt c = ord c - ord '0'

parse :: String -> [[Int]]
parse t = map (map charToInt) $ lines t


visibleScan :: [Int] -> [Bool]
visibleScan = map fst . tail . scanl next (True, -1)
  where
    next :: (Bool, Int)  -- (visible, tallest tree so far)
         -> Int          -- current tree
         -> (Bool, Int)
    next (_, m) t = (m < t, max m t)

visible :: [[Int]] -> [[Bool]]
visible trees =
  visibleFromLeft trees `maskOr`
    visibleFromRight trees `maskOr`
    visibleFromTop trees `maskOr`
    visibleFromBottom trees
  where
    maskOr = zipWith (zipWith (||))
    visibleFromLeft = map visibleScan
    visibleFromRight = map (reverse . visibleScan . reverse)
    visibleFromTop = transpose . map visibleScan . transpose
    visibleFromBottom = transpose . map (reverse . visibleScan . reverse) . transpose

solveA :: [[Int]] -> Int
solveA trees =
  sum $ map (length . filter id) $ visible trees

-- assumes arguments are in range
rays :: Int -> Int -> [[a]] -> ([a], [a], [a], [a])
rays row col trees = (raysUp, raysDown, raysLeft, raysRight)
  where
    raysUp = reverse $ take row $ transpose trees !! col
    raysDown = drop (row + 1) $ transpose trees !! col
    raysLeft = reverse $ take col $ trees !! row
    raysRight = drop (col + 1) $ trees !! row

takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd _ [] = []
takeWhileEnd p (x:xs)
  | p x       = x : takeWhileEnd p xs
  | otherwise = [x]

solveB :: [[Int]] -> Int
solveB trees =
  maximum [ scenicScore i j
          | i <- [0..length trees - 1]
          , j <- [0..length (head trees) - 1]
          ]
  where
    scenicScore row col =
      product $ map visible_trees rays_list
      where
        height = trees !! row !! col
        (rays_up, rays_down, rays_left, rays_right) = rays row col trees
        rays_list = [rays_up, rays_down, rays_left, rays_right]
        visible_trees = length . takeWhileEnd (< height)

solutions :: [String -> String]
solutions = map (\s -> show . s . parse) [solveA, solveB]
