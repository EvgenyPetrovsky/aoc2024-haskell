{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day18
  ( Problem (..),
    Answer (..),
  )
where

import qualified Data.Set as Set (Set, fromList, member, map, notMember)
import Lib (Answer (..))
import qualified Lib as L (Problem (..))
import Util ((|>))
import Algorithm.Search (dijkstraAssoc)
import Data.Foldable (Foldable(toList))

type Location = (Int, Int)
type State = Location
type Cost = Int

data Field = Field
  { walls :: [Location],
    track :: [Location],
    judge :: Location
  }
  deriving (Show)

data Problem = Input String | Problem Field

instance Show Problem where
  show (Input input) = "Unparsed input: \n" ++ input
  show (Problem field) =
    [[posToChar (x, y) | x <- [0 .. max_x]] | y <- [0 .. max_y]]
      |> unlines
    where
      walls_field = Set.fromList $ walls field
      track_field = Set.fromList $ track field
      max_x = map fst (walls field) |> maximum
      max_y = map snd (walls field) |> maximum
      judge_pos = judge field
      posToChar :: (Int, Int) -> Char
      posToChar pos
        | pos == (0, 0) = 'S'
        | pos == (max_x, max_y) = 'E'
        | pos == judge_pos = '●'
        | pos `Set.member` walls_field = '█' -- ▨▩▮▬▦▰●■■∎'
        | pos `Set.member` track_field = '○'
        | otherwise = '·'

instance L.Problem Problem where
  parse1 = parsePart1
  solve1 = solvePart1
  solve2 = solvePart2

--------------------------------------------------------------------------------

solvePart2 :: Problem -> Answer
solvePart2 (Input input) = solvePart2 $ parsePart1 input
solvePart2 (Problem field) =
  let nmax = iterate 0
      max_walls = take (nmax - 1) all_walls
      (_,last_track) = solve (Set.fromList max_walls) start end |> toList |> head
      judgement_byte = last . take nmax $ all_walls
      delim = "\n" ++ replicate (max_x+1) '-'
      solved_field = field {track = last_track, judge = judgement_byte, walls = max_walls}
  in AnswerTxt $ "Cutting byte:" ++ show judgement_byte ++ delim ++ "\n" ++ show (Problem solved_field) ++ delim

  --in AnswerInt $ iterate 0
  where
    max_x = map fst (walls field) |> maximum
    max_y = map snd (walls field) |> maximum
    start = (0,0) :: State
    end = (max_x,max_y) :: Location
    all_walls = walls field
    iterate :: Int -> Int
    iterate n =
      case solve (Set.fromList $ take n all_walls) start end of
        Nothing -> n
        Just _ -> iterate (n+1)

   --in AnswerTxt $ show (Problem problem_field)

--------------------------------------------------------------------------------

solvePart1 :: Problem -> Answer
solvePart1 (Input input) = solvePart1 $ parsePart1 input
solvePart1 (Problem field) =
  let
      max_x = map fst (walls field) |> maximum
      max_y = map snd (walls field) |> maximum
      take_size = if max_x < 70 then 12 else 1024
      problem_field = field {walls = take take_size $ walls field }
      start = (0,0) :: State
      end = (max_x,max_y) :: Location
      (min_cost, shortest_track) = solve (Set.fromList $ walls problem_field) start end |> toList |> head
      solved_field = problem_field {track = shortest_track}
      delim = "\n" ++ replicate (max_x+1) '-'
   in AnswerTxt $ "Number of steps:" ++ show min_cost ++ delim++ "\n" ++ show (Problem solved_field) ++ delim
   --in AnswerTxt $ show (Problem problem_field)

solve :: Set.Set Location -> State -> Location -> Maybe (Cost, [State])
solve walls start_state end_location =
  dijkstraAssoc next found start_state
  where
    (max_x, max_y) = end_location
    fixed_cost = 1 :: Cost
    found :: State -> Bool
    found pos = pos == end_location
    next :: State -> [(State, Cost)]
    next (x,y) =
      [ ((nx, ny), fixed_cost) | (nx, ny) <- [ (x + dx, y + dy) | (dx,dy) <- [(-1,0), (1,0), (0,-1), (0,1)]]
                               , nx >= 0, nx <= max_x, ny >= 0, ny <= max_y
                               , (nx,ny) `Set.notMember` walls]

--------------------------------------------------------------------------------

parsePart1 :: String -> Problem
parsePart1 input =
  let all_lines = lines input
   in all_lines
        |> map (map (\c -> if c == ',' then ' ' else c))
        |> map words
        |> map (\ws -> (read (head ws) :: Int, read (ws !! 1) :: Int))
        |> (\x -> Problem Field {walls = x, track = [], judge = (-1,-1)})

--------------------------------------------------------------------------------
