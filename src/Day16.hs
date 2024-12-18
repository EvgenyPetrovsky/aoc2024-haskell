{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE TupleSections #-}

module Day16
  ( Problem (..),
    Answer (..),
  )
where

import Algorithm.Search (dijkstraAssoc)
import Data.Foldable (Foldable (toList))
import Data.List (nub)
import qualified Data.Set as Set (Set, empty, fromList, map, member, notMember)
import Lib (Answer (..))
import qualified Lib as L (Problem (..))
import Util ( (|>) )

data Turn = L | R deriving (Eq, Show)

data Direction = N | E | S | W deriving (Eq, Show, Ord)

instance Enum Direction where
  toEnum 0 = N
  toEnum 1 = E
  toEnum 2 = S
  toEnum 3 = W
  toEnum x = toEnum $ x `mod` 4
  fromEnum N = 0
  fromEnum E = 1
  fromEnum S = 2
  fromEnum W = 3
  succ = toEnum . (\x -> (x + 1) `mod` 4) . fromEnum
  pred = toEnum . (\x -> (x - 1) `mod` 4) . fromEnum

type Location = (Int, Int)

type Cost = Int

-- type StateC = (Position, Cost)
type State = Position

data Field = Field
  { walls :: Set.Set Location,
    start :: Location,
    end :: Location,
    paths :: Set.Set Location
  }

data Problem = Input String | Problem Field

type Position = Int

toPos :: (Location, Direction) -> Position
toPos ((row, col), dir) =
  row * 10000 + col * 10 + fromEnum dir

posTurnRt :: Position -> Position
posTurnRt pos =
  let (loc, dir) = divMod pos 10
      new_dir = mod (dir + 1) 4
   in loc * 10 + new_dir

posTurnLt :: Position -> Position
posTurnLt pos =
  let (loc, dir) = divMod pos 10
      new_dir = mod (dir - 1) 4
   in loc * 10 + new_dir

posMoveFw :: Position -> Position
posMoveFw pos =
  case mod pos 10 of
    0 -> pos - 10000
    1 -> pos + 10
    2 -> pos + 10000
    3 -> pos - 10
    _ -> error $ "unknown case, direction is encoded with " ++ show (mod pos 10)

posToLoc :: Position -> Location
posToLoc pos = (pos `div` 10) `divMod` 1000

instance Show Problem where
  show (Input input) = "Unparsed input: \n" ++ input
  show (Problem field) =
    [[posToChar (x, y) | y <- [0 .. max_y]] | x <- [0 .. max_x]]
      |> unlines
    where
      max_x = Set.map fst (walls field) |> maximum
      max_y = Set.map snd (walls field) |> maximum
      posToChar :: (Int, Int) -> Char
      posToChar pos
        | pos `Set.member` walls field = '#'
        | pos `Set.member` paths field = 'O'
        | pos == start field = 'S'
        | pos == end field = 'E'
        | otherwise = '.'

instance L.Problem Problem where
  parse1 = parsePart1
  solve1 = solvePart1
  solve2 = solvePart2

--------------------------------------------------------------------------------

solvePart2 :: Problem -> Answer
solvePart2 (Input input) = solvePart2 $ parsePart1 input
solvePart2 (Problem field) =
  discoverAll opti_tiles tiles_to_discover |> length |> AnswerInt
  where
    wall_tiles = walls field
    (opticost, optistates) = solve wall_tiles (toPos (start field, E)) (end field) |> toList |> head
    opti_tiles = map posToLoc optistates
    max_x = Set.map fst wall_tiles |> maximum
    max_y = Set.map snd wall_tiles |> maximum
    tiles_to_discover =
      [ (x, y) | y <- [0 .. max_y], x <- [0 .. max_x], (x, y) `Set.notMember` walls field, (x, y) `notElem` opti_tiles
      ]
    discoverAll :: [Location] -> [Location] -> [Location]
    discoverAll opti [] = opti
    discoverAll opti (candi : candis) =
      let (cost1, path1) = solve wall_tiles (toPos (start field, E)) candi |> toList |> head
          path1_tiles = map posToLoc path1
          start_pos2 = last path1
          (cost2, path2) = solve wall_tiles start_pos2 (end field) |> toList |> head
          path2_tiles = map posToLoc path2
          new_opti = nub $ path1_tiles ++ path2_tiles ++ opti
          new_candis = filter (`notElem` new_opti) candis
       in if cost1 + cost2 == opticost
            then discoverAll new_opti new_candis
            else discoverAll opti candis

solve :: Set.Set Location -> Position -> Location -> Maybe (Cost, [State])
solve walls start_state end_location =
  dijkstraAssoc next found start_state
  where
    found :: State -> Bool
    found pos = posToLoc pos == end_location
    next :: State -> [(State, Cost)]
    next pos =
      if posToLoc forw_pos `Set.member` walls
        then turns
        else (forw_pos, move_cost) : turns
      where
        turn_cost = 1000
        move_cost = 1
        forw_pos = posMoveFw pos
        turns = [(posTurnLt pos, turn_cost), (posTurnRt pos, turn_cost)]

--------------------------------------------------------------------------------

solvePart1 :: Problem -> Answer
solvePart1 (Input input) = solvePart1 $ parsePart1 input
solvePart1 (Problem field) =
  case solve (walls field) (toPos (start field, E)) (end field) of
    Nothing -> AnswerStr "Something went wrong and solution was not found :-("
    Just (cost, _) -> AnswerInt cost

--------------------------------------------------------------------------------

parsePart1 :: String -> Problem
parsePart1 input =
  Problem (Field {walls = findAll '#', start = findFirst 'S', end = findFirst 'E', paths = Set.empty})
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
