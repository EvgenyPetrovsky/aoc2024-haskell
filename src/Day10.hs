{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Day10
    ( Problem (..)
    , Answer (..)
    ) where

import qualified Lib as L ( Problem (..) )
import Lib ( Answer (..) )
import qualified Data.Map.Strict as Map
import Util
import Data.Char (digitToInt)

data Block = E | B Int deriving (Show)

type Location = (Int, Int)
type Trail = [Location]
type Elevation = Int
type Field = Map.Map Location Elevation

data Problem = Input String | Problem Field
-- instance Show Problem where
--     show (Input s) = show s
--     show (Problem p) = error "show method for Problem is not defined"

instance L.Problem Problem where
    parse1 = parsePart1
    solve1 = solvePart1
    solve2 = solvePart2

--------------------------------------------------------------------------------

solvePart2 :: Problem -> Answer
solvePart2 (Input input) = solvePart2 $ parsePart1 input
solvePart2 (Problem field) =
    field |>
        findTrailHeads |>
        map (\h -> scoutTrails [h] field |> length) |>
        sum |>
        AnswerInt

--------------------------------------------------------------------------------
solvePart1 :: Problem -> Answer
solvePart1 (Input input) = solvePart1 $ parsePart1 input
solvePart1 (Problem field) =
    field |>
        findTrailHeads |>
        map (\h -> scoutTrails [h] field |> map head |> distinct |> length) |>
        sum |>
        AnswerInt

findTrailHeads :: Field -> [Location]
findTrailHeads f = Map.filter (== 0) f |> Map.keys



scoutTrails :: Trail -> Field -> [Trail]
scoutTrails path_to_location field =
    let location = head path_to_location
        (row,col) = location
        location_elevation :: Elevation
        location_elevation = field Map.! location
        locations_around =
            [(-1,0),(0,1),(1,0),(0,-1)] |>
            map (\(dr,dc) -> (row + dr, col + dc))
        neighbour_locations = filter (`Map.member` field) locations_around
        steady_ascends = filter (\l -> field Map.! l == location_elevation + 1) neighbour_locations
    in steady_ascends |> concatMap (
        \l ->
            if l `isTrailTop` field then [l:path_to_location]
            else scoutTrails (l:path_to_location) field)
    where
        isTrailTop :: Location -> Field -> Bool
        isTrailTop l f = f Map.! l == 9

--------------------------------------------------------------------------------

parsePart1 :: String -> Problem
parsePart1 input =
    [((row, col), digitToInt dgt)
        | (row, ln)  <- zip [1..] (lines input)
        , (col, dgt) <- zip [1..] ln] |> Map.fromList |> Problem