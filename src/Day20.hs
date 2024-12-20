{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE TupleSections #-}

module Day20
  ( Problem (..),
    Answer (..),
  )
where

import Algorithm.Search (dijkstraAssoc)
import qualified Data.Set as Set (Set, empty, fromList, map, member, notMember, delete)
import Lib (Answer (..))
import qualified Lib as L (Problem (..))
import Util ( (|>), adjacents2D, filterNot )
import Data.Foldable (Foldable(toList))
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Debug.Trace (trace)

type Location = (Int, Int)

type Cost = Int

type State = Location

data Field = Field
  { walls :: Set.Set Location,
    start :: Location,
    end :: Location,
    path :: Set.Set Location
  }

data Problem = Input String | Problem Field

instance Show Problem where
  show (Input input) = "Unparsed input: \n" ++ input
  show (Problem field) =
    [[posToChar (x, y) | y <- [0 .. max_y]] | x <- [0 .. max_x]]
      |> unlines
    where
      max_x = Set.map fst (walls field) |> maximum
      max_y = Set.map snd (walls field) |> maximum
      start_pos = start field
      end_pos = end field
      walls_field = walls field
      shortest_track = path field
      cheat_pos = (-1,-1)
      posToChar :: (Int, Int) -> Char
      posToChar pos
        | pos == start_pos = 'S'
        | pos == end_pos = 'E'
        | pos `Set.member` walls_field = '█' -- ▨▩▮▬▦▰●■■∎'
        | pos `Set.member` shortest_track = '○'
        | pos == cheat_pos = '●'
        | otherwise = '·'

instance L.Problem Problem where
  parse1 = parsePart1
  solve1 = solvePart1
  solve2 = solvePart2

--------------------------------------------------------------------------------

solvePart2 :: Problem -> Answer
solvePart2 (Input input) = solvePart2 $ parsePart1 input
solvePart2 (Problem field) =
  let alt_costs = altCosts cheat_duration saving field
      num_cheats = alt_costs |> length
   in AnswerTxt $
      --" original length: " ++ show org_cost ++
      "\n cheating duration: " ++ show cheat_duration ++
      "\n time to save: " ++ show saving ++
      "\n answer: " ++ show num_cheats
  where
    saving = 100
    cheat_duration = 20

altCosts :: Int -> Int -> Field -> [Cost]
altCosts cheat_duration saving field =
  alt_costs
  where
    field_walls = walls field
    start_pos = start field
    end_pos = end field
    (org_cost, org_path) = solve (walls field) start_pos end_pos |> toList |> head
    cmp_path = (start_pos:org_path) |> (\p -> zip p [0..]) |> Map.fromList
    rem_path = reverse (start_pos:org_path) |> (\p -> zip p [0..]) |> Map.fromList
    portals = (start_pos:org_path)
      -- |>
      |> concatMap (
        \l ->
          trace
            ("Processing position " ++ show l ++ " with number " ++ show (cmp_path Map.! l) ++ " on the path")
            (l `adj2dOrto` cheat_duration)
          |> filter (`isValid` field_walls)
          |> filter (\l' -> rem_path Map.! l' <= rem_path Map.! l - saving)
          |> map (\l' -> (l,l')))
    alt_costs =
      portals
      |> map (\(from, to) -> cmp_path Map.! from + rem_path Map.! to + distance from to)
      |> filter (<= (org_cost - saving))
      -- |> map

--------------------------------------------------------------------------------

{--
  the path occupies all field. knowing the remaining lenght for location and length of path behid, we can
  - make modifications of neighbours (location_a's) in location_p
  - move to location_n and increase length by 1
  - discover neighbours (skipping original) location_n and remaining length
  - hold only options where length to location_p + 1 + 1 + remaining length for location_n < original length
-}

solvePart1 :: Problem -> Answer
solvePart1 (Input input) = solvePart1 $ parsePart1 input
solvePart1 (Problem field) =
  let num_cheats = alt_costs |> filter (<= (org_cost - saving)) |> length
      num_cheats_precise = alt_costs |> filter (== (org_cost - saving)) |> length
   in AnswerTxt $
      " original length: " ++ show org_cost ++
      "\n cheating duration: 2" ++
      "\n time to save: " ++ show saving ++
      "\n number of cheats saving exact time: " ++ show num_cheats_precise ++
      "\n answer: " ++ show num_cheats
  where
    _ = trace ("original length: " ++ show org_cost)
    saving = 100
    field_walls = walls field
    start_pos = start field
    end_pos = end field
    (org_cost, org_path) = solve (walls field) start_pos end_pos |> toList |> head
    isWall :: Location -> Bool
    isWall l = l `Set.member` field_walls
    cmp_path :: Map.Map Location Cost
    cmp_path = (start_pos:org_path) |> (\p -> zip p [0..]) |> Map.fromList
    rem_path :: Map.Map Location Cost
    rem_path = reverse (start_pos:org_path) |> (\p -> zip p [0..]) |> Map.fromList
    --alt_costs :: [Cost]
      -- find where do we get by stepping off the original poath into wall and making one more step outisde the wall
      -- filter out cases when alternative location equals to original location
    alt_costs =
      (start_pos:org_path)
      |> concatMap (\loc -> adjacents2D loc |> filter isWall
        |> concatMap (\loc1 -> next field_walls loc1 |> map fst |> filterNot isWall |> map (\loc2 -> (loc, loc2))))
      |> filter (uncurry (/=))
      |> nub
      |> map (\(loc, alt) -> cmp_path Map.! loc + 2 + rem_path Map.! alt)


solve :: Set.Set Location -> State -> Location -> Maybe (Cost, [State])
solve walls start_state end_location =
  dijkstraAssoc (next walls) found start_state
  where
    found :: State -> Bool
    found pos = pos == end_location


next :: Set.Set Location -> State -> [(State, Cost)]
next walls loc =
  let fixed_cost = 1 :: Cost
   in [ (l', fixed_cost) | l' <- adj2dOrto loc 1, l' `isValid` walls]

distance :: Location -> Location -> Int
distance (x1,y1) (x2,y2) = abs(x1-x2) + abs(y1-y2)

isValid :: Location -> Set.Set Location -> Bool
isValid location walls =
  let (x,y) = location
      max_x = Set.map fst walls |> maximum
      max_y = Set.map snd walls |> maximum
   in x >= 0 && x <= max_x && y >= 0 && y <= max_y && location `Set.notMember` walls

adj2dOrto :: Location -> Int -> [Location]
adj2dOrto (x,y) distance =
  let range = [-distance..distance]
   in [(x+dx,y+dy) | dx <- range
                   , dy <- range
                   , (abs dx + abs dy) <= distance]

adj2dAll :: Location -> Int -> [Location]
adj2dAll (x,y) distance =
  let range = [-distance..distance]
   in [(x+dx,y+dy) | dx <- range, dy <- range]

--------------------------------------------------------------------------------

parsePart1 :: String -> Problem
parsePart1 input =
  Problem (Field {walls = findAll '#', start = findFirst 'S', end = findFirst 'E', path = Set.empty})
  where
    map_lines = lines input
    charLocations1D :: Char -> String -> [Int]
    char `charLocations1D` string = zip [0 ..] string |> filter (\(_, c) -> c == char) |> map fst
    charLocations2D :: Char -> [String] -> [(Int, Int)]
    char `charLocations2D` strings =
      zip [0 ..] strings |> concatMap (\(i1, s) -> char `charLocations1D` s |> map (i1,))
    findFirst char = head (char `charLocations2D` map_lines)
    findAll char = char `charLocations2D` map_lines |> Set.fromList

--------------------------------------------------------------------------------
