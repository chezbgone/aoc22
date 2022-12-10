{-# LANGUAGE OverloadedStrings #-}
module Day07 where

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Void

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

data LsOutput = ShownDirectory String | ShownFile Int String
data CommandOutput = CD String | LS [LsOutput]
type ExecutionHistory = [CommandOutput]

-------------
-- Parsing --
-------------

type Parser = P.Parsec Void Text

cdParser :: Parser CommandOutput
cdParser = do
  _ <- P.string "$ cd "
  CD <$> P.manyTill P.anySingle P.newline

lsOutputParser :: Parser LsOutput
lsOutputParser =
  ShownDirectory <$> (P.string "dir " *> P.manyTill P.anySingle P.newline) <|>
    ShownFile <$> L.decimal <*> (P.space *> P.manyTill P.anySingle P.newline)

lsParser :: Parser CommandOutput
lsParser = do
  _ <- P.string "$ ls\n"
  LS <$> P.many lsOutputParser

parser :: Parser ExecutionHistory
parser = P.many commandParser
  where commandParser = cdParser <|> lsParser

parse :: Text -> ExecutionHistory
parse = fromJust . P.parseMaybe parser

------------------------
-- Building Structure --
------------------------

data FileSystemNode = Directory String (Set FileSystemNode) | File String Int
  deriving (Eq, Ord)

data FileSystemContext =
  DirectoryContext
    String                -- parent dir name
    (Set FileSystemNode)  -- sibling nodes

type FileSystemFocus =
  ( String               -- current dir name
  , Set FileSystemNode   -- files in current dir
  , [FileSystemContext]  -- layers around current dir
  )

isDirectory :: String -> FileSystemNode -> Bool
isDirectory name fsnode =
  case fsnode of
    Directory s _ -> name == s
    _ -> False

updateDirectoryList :: Set FileSystemNode -> [LsOutput] -> Set FileSystemNode
updateDirectoryList = foldl' update
  where
    update :: Set FileSystemNode -> LsOutput -> Set FileSystemNode
    update fs upd =
      case upd of
        ShownDirectory dir ->
          if null $ S.filter (isDirectory dir) fs then
            -- dir is not in fs
            Directory dir S.empty `S.insert` fs
          else
            -- dir already in fs, don't override
            fs
        ShownFile size name -> File name size `S.insert` fs

--        o    --                        o      --
--       / \    |                     /     \    | outer
--      o   o   | context          parent    o  --
--     / \      |                 /  \  \
--   cwd  o    --               cwd  siblings
--   / \                        / \
--  files                      files
updateFS :: FileSystemFocus -> CommandOutput -> FileSystemFocus
updateFS (cwd, files, context) command =
  case command of
    CD ".." ->
      case context of
        [] -> error "'/' has no parent"
        DirectoryContext par siblings: outer ->
          (par, Directory cwd files `S.insert` siblings, outer)
    CD "/" ->
      case context of
        [] -> (cwd, files, context)
        _ -> (cwd, files, context) `updateFS` (CD "..") `updateFS` (CD "/")
    CD dir ->
      let (dirs, other_contents) = S.partition (isDirectory dir) files
      in case S.size dirs of
           1 -> case S.elemAt 0 dirs of
                  Directory _ contents ->
                    (dir, contents, (DirectoryContext cwd other_contents):context)
                  _ -> error "cannot cd into file"
           0 -> error $ dir <> " directory not found"
           _ -> error "multiple files with name found"
    LS outputs -> (cwd, updateDirectoryList files outputs, context)

buildTree :: ExecutionHistory -> FileSystemFocus
buildTree = foldl' updateFS initial_focus
  where initial_focus = ("/", S.empty, [])

wholeFileSystem :: FileSystemFocus -> FileSystemNode
wholeFileSystem (cwd, files, context) =
  case context of
    [] -> Directory cwd files
    DirectoryContext par siblings : ps ->
      wholeFileSystem (par, Directory cwd files `S.insert` siblings, ps)

-------------
-- Solving --
-------------

sizeAccum :: FileSystemNode
       -> ( Int  -- sum of files inside
          , Int  -- recursive sum of directories with size <= 100_000
          )
sizeAccum fsNode =
  case fsNode of
    File _ size -> (size, 0)
    Directory _ files ->
      let
        (sums, recs) = unzip $ sizeAccum <$> S.toList files
        total = sum sums
      in (total, sum recs + if total <= 100000 then total else 0)

solveA :: ExecutionHistory -> Int
solveA hist = snd $ sizeAccum tree
  where tree = wholeFileSystem $ buildTree hist

fsSizes :: FileSystemNode
       -> ( Int     -- sum of files inside
          , Set Int -- sizes of directories and subdirectories
          )
fsSizes fsNode =
  case fsNode of
    File _ size -> (size, S.empty)
    Directory _ files ->
      let
        (sums, recs) = unzip $ fsSizes <$> S.toList files
        total = sum sums
      in (total, total `S.insert` S.unions recs)

solveB :: ExecutionHistory -> Int
solveB hist = fromJust $ S.lookupGE goal sizes
  where
    tree = wholeFileSystem $ buildTree hist
    (total, sizes) = fsSizes tree
    goal = total - 40_000_000


solutions :: [Text -> String]
solutions = map (\s -> show . s . parse) [solveA, solveB]
