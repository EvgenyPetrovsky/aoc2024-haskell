{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day08
  ( Problem (..),
    Answer (..),
  )
where

import qualified Data.Set as Set
-- import qualified Data.Map as Map

import Lib (Answer (..))
import qualified Lib as L (Problem (..))
import Util

type Label = Char

type Location = (Int, Int)

-- type Answer = L.Answer
type Antena = (Label, Location)

data Field = Field
  { rows :: Int,
    columns :: Int,
    antenas :: [Antena]
  }
  deriving (Show)

data Problem = Input String | Problem Field deriving (Show)

instance L.Problem Problem where
  parse1 = parsePart1
  solve1 = solvePart1
  solve2 = solvePart2

--------------------------------------------------------------------------------

solvePart2 :: Problem -> Answer
solvePart2 (Input input) = solvePart2 . L.parse2 $ input
solvePart2 (Problem field) =
  let antenas' = antenas field
      rows' = field |> rows
      cols' = field |> columns
      labels = map fst antenas' |> Set.fromList |> Set.toList
   in labels
        |> map (`locations` antenas')
        |> concatMap antinodes2
        |> filter (`withinBounds` (rows', cols'))
        |> Set.fromList
        |> Set.toList
        |> length
        |> AnswerInt

solvePart1 :: Problem -> Answer
solvePart1 (Input input) = solvePart1 . L.parse1 $ input
solvePart1 (Problem field) =
  let antenas' = antenas field
      rows' = field |> rows
      cols' = field |> columns
      labels = map fst antenas' |> Set.fromList |> Set.toList
   in labels
        |> map (`locations` antenas')
        |> concatMap antinodes
        |> filter (`withinBounds` (rows', cols'))
        |> Set.fromList
        |> Set.toList
        |> length
        |> AnswerInt

locations :: Label -> [Antena] -> [Location]
locations label antenas = antenas |> filter (\(a, _) -> a == label) |> map snd

antinodes :: [Location] -> [Location]
antinodes [] = []
antinodes ((r0, c0) : ls) =
  let this = [(r0 + d * (r - r0), c0 + d * (c - c0)) | (r, c) <- ls, d <- [-1, 2]]
   in this ++ antinodes ls

antinodes2 :: [Location] -> [Location]
antinodes2 [] = []
antinodes2 ((r0, c0) : ls) =
  let this = [(r0 + d * (r - r0), c0 + d * (c - c0)) | (r, c) <- ls, d <- [-50 .. 50]]
   in this ++ antinodes2 ls

withinBounds :: Location -> (Int, Int) -> Bool
withinBounds (r, c) (rows, cols) = 0 < r && r <= rows && 0 < c && c <= cols

--------------------------------------------------------------------------------

parsePart1 :: String -> Problem
parsePart1 input =
  let ls = lines input
      rows = length ls
      cols = length $ head ls
      ants =
        [ (a, (r, c)) | (r, l) <- zip [1 ..] ls, (c, a) <- zip [1 ..] l, a /= '.'
        ]
   in Problem $ Field rows cols ants
