module Day04
  ( Problem (..)
  ) where

import qualified Lib ( Problem(..) )
import Lib (Answer (..))
import Util


newtype Problem = Problem [[Char]]
instance Lib.Problem Problem where
  parse1 input = lines input |> Problem
  solve1 (Problem lss) =
    let
      rows = length lss
      cols = length (head lss)
      rcs = [(r,c) | r <- [0..], r < rows, c <- [0..], c < cols]
    in
        map (`countXMAS` lss) rcs |> sum |> AnswerInt
  solve2 _ = Unknown :: Answer


{------------------------------------------------------------------------------}

type Placement = [(Int, Int)]
placements :: [Placement]
placements =
  let
    directions   = [(0,1), (1,1), (1,0), (1,-1), (0,-1), (-1,-1), (-1,0), (-1,1)]
    displacement = [0..3]
  in map (\(r,c) -> map (\i -> (r*i,c*i)) displacement) directions

isValidPlacement :: Placement -> Int -> Int -> Bool
isValidPlacement p rows cols =
  all (\(r,c) -> 0 <= r && r < rows && 0 <= c && c < cols) p

readWord :: Placement -> [[Char]] -> String
readWord p lss = map (\(r, c) -> lss!!r!!c) p

countXMAS :: (Int, Int) -> [[Char]] -> Int
countXMAS (r,c) lss =
  let
    rows = length lss
    cols = length (head lss)
  in
    placements |>
    map (map (\(dr, dc) -> (r+dr, c+dc))) |>
    filter (\p -> isValidPlacement p rows cols) |>
    map (`readWord` lss) |>
    filter (== "XMAS") |> length
