module Day12 where

import Control.Monad
import Control.Monad.State
import Data.Char
import Data.Maybe
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Vector (Vector)
import Data.Vector qualified as V

type Grid = Vector (Vector Int)
type Coordinate = (Int, Int)

-------------
-- Parsing --
-------------

parse :: String -> (Grid, Coordinate, Coordinate)
parse t = (V.map (V.map height_from_char) grid, find 'S', find 'E')
  where
    grid = V.fromList $ map V.fromList $ lines t
    height_from_char c =
      case c of
        'S' -> height_from_char 'a'
        'E' -> height_from_char 'z'
        _   -> ord c - ord 'a'
    find c = fromJust $ do
      row <- V.findIndex (V.elem c) grid
      col <- V.elemIndex c (grid V.! row)
      pure (row, col)

-----------
-- Solve --
-----------

data BFSState =
  BFSState { visited :: Set Coordinate
           , toVisit :: Seq Coordinate
           , aux :: Map Coordinate Int  -- distance from start
           }

bfsStep :: Grid -> (Grid -> Coordinate -> [Coordinate]) -> BFSState -> BFSState
bfsStep grid neighbors = execState $ do
  BFSState visited toVisit aux <- get
  case Seq.viewl toVisit of
    Seq.EmptyL                -> error "nothing to visit"
    (currentNode Seq.:< rest) -> do
      let nextNodes = filter (`S.notMember` visited) (neighbors grid currentNode)
      let newToVisit = rest Seq.>< Seq.fromList nextNodes
      let newVisited = S.union visited (S.fromList nextNodes)
      let currentDist = aux M.! currentNode
      let newAux = M.union aux (M.fromList $ map (,currentDist+1) nextNodes)
      put $ BFSState newVisited newToVisit newAux

neighborsA :: Grid -> Coordinate -> [Coordinate]
neighborsA grid (x, y) = do
  (dx, dy) <- [(-1, 0), (0, -1), (1, 0), (0, 1)]
  guard $ 0 <= x + dx
  guard $ 0 <= y + dy
  guard $ x + dx < V.length grid
  guard $ y + dy < V.length (V.head grid)
  guard $ (grid V.! (x+dx) V.! (y+dy)) <= (grid V.! x V.! y) + 1
  pure (x + dx, y + dy)

solveA :: (Grid, Coordinate, Coordinate) -> Int
solveA (grid, start, end) = flip evalState initialState $ do
  let not_done s = not $ end `M.member` aux s
  while (not_done <$> get) $ do
    modify $ bfsStep grid neighborsA
  distances <- aux <$> get
  pure $ distances M.! end
  where
    initialState =
      BFSState S.empty (Seq.singleton start) (M.singleton start 0)
    while cond body = do
      b <- cond
      when b (body >> while cond body)

-- neighbors going backward
neighborsB :: Grid -> Coordinate -> [Coordinate]
neighborsB grid (x, y) = do
  (dx, dy) <- [(-1, 0), (0, -1), (1, 0), (0, 1)]
  guard $ 0 <= x + dx
  guard $ 0 <= y + dy
  guard $ x + dx < V.length grid
  guard $ y + dy < V.length (V.head grid)
  guard $ (grid V.! (x+dx) V.! (y+dy)) + 1 >= (grid V.! x V.! y)
  pure (x + dx, y + dy)

-- do bfs backward from the goal until every beginning location reached
solveB :: (Grid, Coordinate, Coordinate) -> Int
solveB (grid, _, end) = flip evalState initialState $ do
  let beginnings = S.fromList $ V.toList $ do
        (i, row) <- V.indexed grid
        (j, cell) <- V.indexed row
        guard $ cell == 0
        pure (i, j)
  let done = Seq.null . toVisit
  while ((not . done) <$> get) $ do
    modify $ bfsStep grid neighborsB
  distances <- aux <$> get
  pure $ minimum $ M.restrictKeys distances beginnings
  where
    initialState =
      BFSState S.empty (Seq.singleton end) (M.singleton end 0)
    while cond body = do
      b <- cond
      when b (body >> while cond body)

solutions :: [String -> String]
solutions = map (\s -> show . s . parse) [solveB]
