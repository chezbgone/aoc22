{-# LANGUAGE OverloadedStrings #-}
module Day08 (solutions) where

import Data.Text (Text)
import Data.Text qualified as T

data DSL

parse :: Text -> DSL
parse t = undefined $ T.lines t

solveA :: DSL -> Int
solveA = undefined

solveB :: DSL -> Int
solveB = undefined

solutions :: [Text -> String]
solutions = map (\s -> show . s . parse) [solveA, solveB]
