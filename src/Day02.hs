{-# LANGUAGE OverloadedStrings #-}
module Day02 (solutions) where

import Data.Text (Text)
import Data.Text qualified as T

data Left = A | B | C deriving Read
data Right = X | Y | Z deriving Read

parseLine :: Text -> (Left, Right)
parseLine t = (read l, read r)
  where
    xs = map T.unpack $ T.words t
    l = xs !! 0 -- avoid incomplete pattern warnings
    r = xs !! 1

parse :: Text -> [(Left, Right)]
parse t = parseLine <$> T.lines t


data Move = R | P | S

winner :: Move -> Move -> Ordering
winner x y =
  case (x, y) of
    (R, R) -> EQ
    (R, P) -> LT
    (R, S) -> GT
    (P, R) -> GT
    (P, P) -> EQ
    (P, S) -> LT
    (S, R) -> LT
    (S, P) -> GT
    (S, S) -> EQ

leftA :: Left -> Move
leftA A = R
leftA B = P
leftA C = S

rightA :: Right -> Move
rightA X = R
rightA Y = P
rightA Z = S

moveScore :: Move -> Int
moveScore R = 1
moveScore P = 2
moveScore S = 3

resultScore :: Ordering -> Int
resultScore LT = 6
resultScore EQ = 3
resultScore GT = 0


scoreA :: (Left, Right) -> Int
scoreA (l, r) = moveScore right + resultScore (winner left right)
  where
    left = leftA l
    right = rightA r

solveA :: [(Left, Right)] -> Int
solveA = sum . map scoreA

findMove :: Move -> Ordering -> Move
findMove l res = head [r | r <- [R, P, S], winner l r == res]

rightB :: Right -> Ordering
rightB X = GT
rightB Y = EQ
rightB Z = LT

scoreB :: (Left, Right) -> Int
scoreB (l, r) = moveScore (findMove left right) + resultScore right
  where
    left = leftA l
    right = rightB r


solveB :: [(Left, Right)] -> Int
solveB = sum . map scoreB

showOutput :: Int -> String
showOutput = show

solutions :: [Text -> String]
solutions = map (\s -> showOutput . s . parse) [solveA, solveB]
