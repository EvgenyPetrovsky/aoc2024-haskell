{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day25
  ( Problem (..),
    Answer (..),
  )
where

import Data.List (transpose, partition)
import Lib (Answer (..))
import qualified Lib as L (Problem (..))
import Util ((|>), filterNot, chunksOf)

type Connection = (String, String)
data Problem = Input String | Problem [Connection]

instance L.Problem Problem where
  parse1 = parsePart1
  solve1 = solvePart1
  solve2 = solvePart2

--------------------------------------------------------------------------------

solvePart2 :: Problem -> Answer
solvePart2 (Input input) = solvePart2 $ parsePart1 input
solvePart2 (Problem _) = Unknown

--------------------------------------------------------------------------------

solvePart1 :: Problem -> Answer
solvePart1 (Input input) = solvePart1 $ parsePart1 input
solvePart1 (Problem (cilinders, keys)) = Unknown

connected :: [Connection] -> [[Connection]]
connected [] = []
connected con:cons =
  let bindings = filter
  if


--------------------------------------------------------------------------------

parsePart1 :: String -> Problem
parsePart1 input =
  input
  |> lines
  |> map (\l -> map (\c -> if c == '-' then ' ' else c) l) all_lines
  |> map words
  |> map (\ws -> (head ws, ws!!1))
  Problem

--------------------------------------------------------------------------------
