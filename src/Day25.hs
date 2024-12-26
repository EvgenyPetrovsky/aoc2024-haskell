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

type Definition = ([[Int]], [[Int]])
data Problem = Input String | Problem Definition

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
solvePart1 (Problem (cilinders, keys)) =
  [(c,k) | c <- cilinders, k <- keys]
  |> filter (\(c,k) -> zipWith (+) c k |> all (<= 7))
  |> length
  |> AnswerInt



--------------------------------------------------------------------------------
parsePart1 :: String -> Problem
parsePart1 input =
  Problem (map blockLen cilinders, map blockLen keys)
  where
    all_lines = lines input |> filterNot (== [])
    blocks = chunksOf 7 all_lines
    (cilinders, keys) = partition (all (== '#') . head) blocks

blockLen :: [String] -> [Int]
blockLen blocklines =
  transpose blocklines |> map (filter (=='#')) |> map length

--------------------------------------------------------------------------------
