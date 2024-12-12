{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day12
  ( Problem (..),
    Answer (..),
  )
where

-- import qualified Data.Map.Strict as Map

import Data.List (partition)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lib (Answer (..))
import qualified Lib as L (Problem (..))
import ParseUtil (inputToLocationCharTupples)
import Util

type Location = (Int, Int)

type Plant = Char

-- type Cell = (Location, Plant)
type Field = Map.Map Location Plant

type Area = Set.Set Location

data Problem = Input String | Problem Field deriving (Show)

instance L.Problem Problem where
  parse1 = parsePart1
  solve1 = solvePart1
  solve2 = solvePart2

--------------------------------------------------------------------------------

solvePart2 :: Problem -> Answer
solvePart2 (Input input) = solvePart2 $ parsePart1 input
solvePart2 (Problem field) =
  let areas = createAreas field
      area_sizes = map areaSize areas
      area_sides = map areaSides areas
   in zipWith (*) area_sizes area_sides |> sum |> AnswerInt

{-
  calcualte number of sides for given area
  scan all rows one by one and calculate number of edger on upper and lower side
  do the same for columns, by calculating edges on left and right side
-}
areaSides :: Area -> Int
areaSides area =
  let ((rmin, cmin), (rmax, cmax)) = boxAroundArea area
      (rows, cols) = ([rmin .. rmax], [cmin .. cmax])
      hor_up_sides = map (\r -> countEdgesOnLine [(r, c) | c <- cols] (-1, 0) area) rows
      hor_dn_sides = map (\r -> countEdgesOnLine [(r, c) | c <- cols] (1, 0) area) rows
      ver_lt_sides = map (\c -> countEdgesOnLine [(r, c) | r <- rows] (0, -1) area) cols
      ver_rt_sides = map (\c -> countEdgesOnLine [(r, c) | r <- rows] (0, 1) area) cols
   in sum (hor_up_sides ++ hor_dn_sides ++ ver_lt_sides ++ ver_rt_sides)

{-
  draw a box around area :
    top left corner is min row & min column,
    bottom right corner is max row & max column.
-}
boxAroundArea :: Area -> (Location, Location)
boxAroundArea area =
  let (rows, cols) = area |> Set.toList |> unzip
      top_left = (minimum rows, minimum cols)
      btm_right = (maximum rows, maximum cols)
   in (top_left, btm_right)

{-
  count continious edges along the line
  new edge begins every time when
    only location is in the area
    and adjactent location is not
    and there was no edge in previous location
-}
countEdgesOnLine :: [Location] -> (Int, Int) -> Area -> Int
countEdgesOnLine line_locations adj_displacement area =
  iterate 0 False line_locations
  where
    isArea :: Location -> Bool
    isArea l = Set.member l area
    adj :: Location -> Location
    adj (r, c) = let (dr, dc) = adj_displacement in (r + dr, c + dc)
    iterate :: Int -> Bool -> [Location] -> Int
    iterate acc _ [] = acc
    iterate acc prev (loc : locs) =
      let (loc_area, adj_area) = (isArea loc, isArea (adj loc))
       in if not prev && loc_area && not adj_area
            then iterate (acc + 1) True locs
            else
              if prev && loc_area && not adj_area
                then iterate acc True locs
                else iterate acc False locs

--------------------------------------------------------------------------------
solvePart1 :: Problem -> Answer
solvePart1 (Input input) = solvePart1 $ parsePart1 input
solvePart1 (Problem field) =
  let areas = createAreas field
      area_sizes = map areaSize areas
      area_perimeters = map areaPerimeter areas
   in zipWith (*) area_sizes area_perimeters |> sum |> AnswerInt

createAreas :: Field -> [Area]
createAreas field =
  foldl (\z l -> addLocationToArea l field z) [] locations
  where
    locations = Map.keys field

areaSize :: Area -> Int
areaSize = Set.size

areaPerimeter :: Area -> Int
areaPerimeter area =
  area |> Set.toList |> map (length . neighbours) |> map (4 -) |> sum
  where
    neighbours :: Location -> [Location]
    neighbours l = locationOrtoNeighbours2d l |> filter (`Set.member` area)

addLocationToArea :: Location -> Field -> [Area] -> [Area]
addLocationToArea location field areas =
  let plant = field Map.! location
      neighbours = locationOrtoNeighbours2d location |> filter (`elem` Map.keys field)
      same_plants = neighbours |> filter (\n -> plant == field Map.! n)
      (areas_neighbours, areas_other) = partition (\a -> any (`Set.member` a) (location : same_plants)) areas
      area_new = foldl Set.union (Set.singleton location) areas_neighbours
   in area_new : areas_other

--------------------------------------------------------------------------------

parsePart1 :: String -> Problem
parsePart1 input = input |> inputToLocationCharTupples |> Map.fromList |> Problem