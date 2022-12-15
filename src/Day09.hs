{-# LANGUAGE OverloadedStrings #-}
module Day09 (solutions) where

import Data.Set (Set)
import Data.Set qualified as S

data Direction = U | D | L | R deriving Read
type Movement = (Direction, Int)
type Position = (Int, Int)
type PositionDelta = (Int, Int)
type Rope = (Position, Position) -- (head, tail)
type RopeB = [Position]

parse :: String -> [Movement]
parse t = map (parse_line . words) $ lines t
  where parse_line xs = (read (xs !! 0), read (xs !! 1))

direction :: Direction -> PositionDelta
direction d = case d of
  U -> (0,  1)
  D -> (0, -1)
  L -> (-1, 0)
  R -> ( 1, 0)

plus_vector :: Position -> PositionDelta -> Position
plus_vector (x, y) (dx, dy) = (x + dx, y + dy)

minus_vector :: Position -> Position -> PositionDelta
minus_vector (x, y) (u, v) = (x - u, y - v)

follow :: Position -- head
       -> Position -- old_follower
       -> Position -- new_follower
follow h old_t@(tx, ty) =
  if -1 <= dx && dx <= 1 && -1 <= dy && dy <= 1 then -- if distance is not big
    old_t
  else
    (tx + move dx, ty + move dy)
  where
    (dx, dy) = minus_vector h old_t
    move :: Int -> Int
    move diff
      | diff < 0   = -1
      | diff == 0  = 0
      | otherwise  = 1

expandMovements :: [Movement] -> [Direction]
expandMovements = go []
  where
    go :: [Direction] -> [Movement] -> [Direction]
    go ds [] = ds
    go ds ((d,n):ms) = replicate n d ++ go ds ms

solve :: forall rope.
         rope
      -> (rope -> Direction -> rope)
      -> (rope -> Position)
      -> [Movement]
      -> Int
solve initial_rope step get_tail =
  S.size . fst . foldl go (S.empty, initial_rope) . expandMovements
  where
    go :: (Set Position, rope)  -- (locations tail visited, position)
       -> Direction
       -> (Set Position, rope)
    go (visited, old_rope) d =
      (get_tail new_position `S.insert` visited, new_position)
      where
        new_position = step old_rope d

stepA :: Rope -> Direction -> Rope
stepA (h, t) d = (new_h, follow new_h t)
  where new_h = h `plus_vector` direction d

solveA :: [Movement] -> Int
solveA = solve initial_rope stepA snd
  where
    initial_position = (0, 0)
    initial_rope = (initial_position, initial_position)

stepB :: RopeB -> Direction -> RopeB
stepB [] _ = []
stepB (h:ps) d = ret
  where
    new_h = h `plus_vector` direction d
    ret = new_h : zipWith follow ret ps

solveB :: [Movement] -> Int
solveB = solve initial_rope stepB last
  where
    initial_position = (0, 0)
    initial_rope :: RopeB
    initial_rope = replicate 10 initial_position

solutions :: [String -> String]
solutions = map (\s -> show . s . parse) [solveA, solveB]
