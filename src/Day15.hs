{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day15
  ( Problem (..),
    Answer (..),
  )
where

import qualified Data.Set as Set (Set, delete, fromList, insert, map, member, union)
import Lib (Answer (..))
import qualified Lib as L (Problem (..))
import Util

-- Field with locations: Robot, Boxes, Walls
type Field = (Location, Set.Set Location, Set.Set Location)

type Location = (Int, Int)

data Object = Wall | Box deriving (Show)

data Direction = N | E | S | W deriving (Show, Eq)

data Problem = Input String | Problem Field [Direction]

instance Show Problem where
  show (Input input) = "Unparsed input: \n" ++ input
  show (Problem (robot, boxes, walls) _) =
    error
      ""
      [[posToChar (x, y) | x <- [0 .. max_x]] | y <- [0 .. max_y]]
      |> unlines
    where
      max_x = Set.map fst walls |> maximum
      max_y = Set.map snd walls |> maximum
      posToChar :: (Int, Int) -> Char
      posToChar pos
        | pos `Set.member` walls = '#'
        | pos `Set.member` boxes = 'O'
        | pos == robot = '@'
        | otherwise = '.'

instance L.Problem Problem where
  parse1 = parsePart1
  parse2 = parsePart2
  solve1 = solvePart1
  solve2 = solvePart2

--------------------------------------------------------------------------------

solvePart2 :: Problem -> Answer
solvePart2 (Input input) = solvePart2 $ parsePart2 input
solvePart2 (Problem field directions) =
  foldl makeMove2 field directions |> checkSum |> AnswerInt

makeMove2 :: Field -> Direction -> Field
makeMove2 field direction
  | not ( canMakeMove2 (next location, boxes, walls) direction ) = field
  | (direction == N || direction == S) && next location `Set.member` boxes =
      let (_, new_b1, _) = makeMove2 (next location, boxes, walls) direction
          (_, new_b2, _) = makeMove2 (right . next $ location, new_b1, walls) direction
          new_b3 = moveBox (next location) new_b2
       in (location, new_b3, walls)
  | (direction == N || direction == S) && left (next location) `Set.member` boxes =
      let (_, new_b1, _) = makeMove2 (next location, boxes, walls) direction
          (_, new_b2, _) = makeMove2 (left . next $ location, new_b1, walls) direction
          new_b3 = moveBox (left . next $ location) new_b2
       in (location, new_b3, walls)
  | direction == E && next location `Set.member` boxes =
      let (_, new_b1, _) = makeMove2 (next . next $ location, boxes, walls) direction
          new_b2 = moveBox (next location) new_b1
       in (location, new_b2, walls)
  | direction == W && next (next location) `Set.member` boxes =
      let (_, new_b1, _) = makeMove2 (next . next $ location, boxes, walls) direction
          new_b2 = moveBox (next . next $ location) new_b1
       in (location, new_b2, walls)
  | otherwise = (next location, boxes, walls)
  where
    (location, boxes, walls) = field
    left (row,col) = (row, col - 1)
    right (row, col) = (row, col + 1)
    next location = applyDirection location direction
    moveBox :: Location -> Set.Set Location -> Set.Set Location
    moveBox in_location in_boxes =
      in_boxes |> Set.delete in_location |> Set.insert (next in_location)

canMakeMove2 :: Field -> Direction -> Bool
canMakeMove2 (location, boxes, walls) direction
  | location `Set.member` walls = False
  -- boxes are represented by only one left coordinate, this is why we must check 2 locations
  -- if box is located up at [(r-1,c), (r-1,c+1)] or down at [(r+1,c), (r+1,c+1)]
  | location `Set.member` boxes && (direction == N || direction == S) =
    canMakeMove2 (next1 location, boxes, walls) direction && canMakeMove2 (next1 location_r, boxes, walls) direction
  -- if box is located up at [(r-1,c-1), (r-1,c)] or down at [(r+1,c-1), (r+1,c)]
  | location_l `Set.member` boxes && (direction == N || direction == S) =
    canMakeMove2 (next1 location, boxes, walls) direction && canMakeMove2 (next1 location_l, boxes, walls) direction
  --
  | location `Set.member` boxes && direction == W = canMakeMove2 (next1 location, boxes, walls) direction
  | location_l `Set.member` boxes && direction == W = canMakeMove2 (next1 location_l, boxes, walls) direction
  --
  | location `Set.member` boxes && direction == E = canMakeMove2 (next1 location_r, boxes, walls) direction
  | location_l `Set.member` boxes && direction == E = canMakeMove2 (next1 location, boxes, walls) direction
  | otherwise = True --error $ "non covered case: location is " ++ show location ++ ", direction is " ++ show direction
  where
    (row, col) = location
    location_l = (row, col - 1)
    location_r = (row, col + 1)
    next1 :: Location -> Location
    next1 location = applyDirection location direction

--------------------------------------------------------------------------------

solvePart1 :: Problem -> Answer
solvePart1 (Input input) = solvePart1 $ parsePart1 input
solvePart1 (Problem field directions) =
  foldl makeMove1 field directions |> checkSum |> AnswerInt

checkSum :: Field -> Int
checkSum (_, boxes, _) =
  Set.map (\(row, col) -> 100 * row + col) boxes |> sum

moveDisplacement :: (Num a, Num b) => Direction -> (a, b)
moveDisplacement d =
  case d of
    N -> (-1, 0)
    E -> (0, 1)
    S -> (1, 0)
    W -> (0, -1)

applyDirection :: Location -> Direction -> Location
applyDirection (row, col) direction =
  let (drow, dcol) = moveDisplacement direction
   in (row + drow, col + dcol)

findNextFreeLocation1 :: (Location, Set.Set Location, Set.Set Location) -> Direction -> Maybe Location
findNextFreeLocation1 (location, boxes, walls) direction
  | location `Set.member` walls = Nothing
  | location `Set.member` boxes = findNextFreeLocation1 (applyDirection location direction, boxes, walls) direction
  | otherwise = Just location

-- canMakeMove :: Field Direction--Location -> Direction -> Set.Set Location -> Set.Set Location -> Bool
-- canMakeMove (this_location, boxes, walls) direction =
--   let new = applyDirection location direction
--   in
--     if new `Set.member` walls then False
--     else new `Set.member` boxes then canMakeMove new direction boxes walls
--     else True

makeMove1 :: Field -> Direction -> Field
makeMove1 (location, boxes, walls) direction
  | new_location `Set.member` walls = (location, boxes, walls)
  | new_location `Set.member` boxes =
      case next_free_location of
        Nothing -> (location, boxes, walls)
        Just free_location -> (new_location, moveBox new_location free_location, walls)
  | otherwise = (new_location, boxes, walls)
  where
    new_location = applyDirection location direction
    next_free_location = findNextFreeLocation1 (new_location, boxes, walls) direction
    moveBox :: Location -> Location -> Set.Set Location
    moveBox from_location to_location =
      boxes |> Set.delete from_location |> Set.insert to_location

--------------------------------------------------------------------------------

parsePart2 :: String -> Problem
parsePart2 input =
  (map widen map_lines ++ dir_lines) |> unlines |> parsePart1
  where
    all_lines = lines input
    (map_lines, dir_lines) = break null all_lines
    widen :: String -> String
    widen [] = []
    widen ('#' : cs) = '#' : '#' : widen cs
    widen ('@' : cs) = '@' : ' ' : widen cs
    widen ('O' : cs) = '[' : ']' : widen cs
    widen (c : cs) = c : c : widen cs

parsePart1 :: String -> Problem
parsePart1 input =
  Problem (findFirst '@', findAll 'O' `Set.union` findAll '[', findAll '#') directions
  where
    all_lines = lines input
    (map_lines, dir_lines) = break null all_lines
    directions = concat dir_lines |> map parseDirection
    charLocations1D :: Char -> String -> [Int]
    char `charLocations1D` string = zip [0 ..] string |> filter (\(_, c) -> c == char) |> map fst
    charLocations2D :: Char -> [String] -> [(Int, Int)]
    char `charLocations2D` strings =
      zip [0 ..] strings |> concatMap (\(i1, s) -> char `charLocations1D` s |> map (\i2 -> (i1, i2)))
    findFirst char = head (char `charLocations2D` map_lines)
    findAll char = char `charLocations2D` map_lines |> Set.fromList

parseDirection :: Char -> Direction
parseDirection c
  | c == '^' = N
  | c == '>' = E
  | c == 'v' = S
  | c == '<' = W
  | otherwise = error $ "unknown direction: " ++ [c]

--------------------------------------------------------------------------------
