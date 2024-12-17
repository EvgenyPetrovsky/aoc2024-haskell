{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day16
  ( Problem (..),
    Answer (..),
  )
where

import Algorithm.Search (dijkstraAssoc)
--import Data.Foldable (Foldable (toList))
import qualified Data.Set as Set (Set, empty, fromList, map, member)
import Lib (Answer (..))
--import GHC.Word (Word8, W8#)
import qualified Lib as L (Problem (..))
import Util

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

type StateC = (Location, Direction, Cost)
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

fromPos :: Position -> (Location, Direction)
fromPos pos =
  let (loc, dir) = divMod pos 10
   in (divMod loc 1000, toEnum dir)

posTurnRt pos =
  let (loc, dir) = divMod pos 10
      new_dir = mod (dir + 1) 4
   in loc * 10 + new_dir

posTurnLt pos =
  let (loc, dir) = divMod pos 10
      new_dir = mod (dir - 1) 4
   in loc * 10 + new_dir

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
    [[posToChar (x , y ) | y <- [0 .. max_y]] | x <- [0 .. max_x]]
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
solvePart2 (Problem field) = Unknown
{-
  -- iterate [] (pth2loc optimal_path) |> distinct |> length |> AnswerInt
  let best_paths_tiles = iterate [] (pth2loc optimal_path) |> distinct
      --rendered_problem = Problem (field {paths = Set.fromList best_paths_tiles}) |> show
   --in AnswerTxt $ "Number of tiles on best paths: " ++ show (length best_paths_tiles + 2) ++ "\n" ++ rendered_problem
   in AnswerInt (length best_paths_tiles + 2)
  where
    org_walls = walls field :: Set.Set Location
    init = (start field, E, 0) :: State
    done = end field
    (optimal_cost, optimal_path) = solve org_walls init done 100000  |> toList |> head
    pth2loc :: [State] -> [Location]
    pth2loc pth = map (\(l,_,_) -> l) pth |> filter (`notElem` [start field, end field])
    iterate :: [Location] -> [Location] -> [Location]
    -- rewrite it: use good & bad locations, test locations for all 4 directions of each non-explored position and
    --  place it under bad if no solution is found
    --  or add all locations of path of good solution
    -- the cost from position is cost from start to position, from position to end
    iterate acc_walls add_walls =
      let new_walls =
            concatMap
              (filter (`notElem` acc_walls))
              [solve (Set.insert w org_walls) init done optimal_cost |> toList |> head |> snd |> pth2loc | w <- add_walls]
       in
        if null new_walls then acc_walls ++ add_walls
        else iterate (acc_walls ++ add_walls) new_walls
-}

solveWithPruning :: Set.Set Location -> StateC -> Location -> Cost -> Maybe (Cost, [StateC])
solveWithPruning walls start_state end_location cutoff_cost =
  dijkstraAssoc nextprn found start_state
  where
    found :: StateC -> Bool
    found (location, _, _) = location == end_location
    nextprn :: StateC -> [(StateC, Cost)]
    nextprn (l, d, c)
      | c > cutoff_cost = []
      | otherwise = if forw_loc `Set.member` walls then turns
                    else ((forw_loc, d, c + move_cost), move_cost) : turns
      where
          turn_cost = 1000
          move_cost = 1
          forw_loc = forward l d
          turns
            = [((l, turn L d, c + turn_cost), turn_cost),
               ((l, turn R d, c + turn_cost), turn_cost)]

solve :: Set.Set Location -> Position -> Location -> Maybe (Cost, [State])
solve walls start_state end_location =
  dijkstraAssoc next found start_state
  where
    found :: State -> Bool
    found pos = posToLoc pos == end_location
    next :: State -> [(State, Cost)]
    next pos =
      if forw_loc `Set.member` walls then turns
      else (forw_pos, move_cost) : turns
      where
        turn_cost = 1000
        move_cost = 1
        forw_pos = posMoveFw pos
        forw_loc = posToLoc forw_pos
        turns = [(posTurnLt pos, turn_cost), (posTurnRt pos, turn_cost)]


--------------------------------------------------------------------------------

solvePart1 :: Problem -> Answer
solvePart1 (Input input) = solvePart1 $ parsePart1 input
solvePart1 (Problem field) =
  -- AnswerTxt $ show (Problem field)
  case solve (walls field) (toPos (start field, E)) (end field) of
  --solve (walls field) (start field, E, 0) (end field) of--
    Nothing -> AnswerStr "Something went wrong and solution was not found :-("
    Just (cost, _) -> AnswerInt cost

moveDisplacement :: Direction -> (Int, Int)
moveDisplacement d =
  case d of
    N -> (-1, 0)
    E -> (0, 1)
    S -> (1, 0)
    W -> (0, -1)

forward :: Location -> Direction -> Location
forward (row, col) direction =
  let (drow, dcol) = moveDisplacement direction
   in (row + drow, col + dcol)

turn :: Turn -> Direction -> Direction
turn t d =
  case (t, d) of
    (R, N) -> E
    (R, E) -> S
    (R, S) -> W
    (R, W) -> N
    (L, N) -> W
    (L, W) -> S
    (L, S) -> E
    (L, E) -> N

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
      zip [0 ..] strings |> concatMap (\(i1, s) -> char `charLocations1D` s |> map (\i2 -> (i1, i2)))
    findFirst char = head (char `charLocations2D` map_lines)
    findAll char = char `charLocations2D` map_lines |> Set.fromList

--------------------------------------------------------------------------------
