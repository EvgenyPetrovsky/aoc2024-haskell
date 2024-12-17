{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day14
  ( Problem (..),
    Answer (..),
  )
where

import qualified Data.Set as Set (Set, fromList, member)
import Lib (Answer (..))
import qualified Lib as L (Problem (..))
import ParseUtil (splitStringAtChar)
import Util

type Location = (Int, Int)

type Velocity = (Int, Int)

type Robot = (Location, Velocity)

data Problem = Input String | Problem [Robot]

instance Show Problem where
  show (Input input) = "Unparsed input: " ++ input
  show (Problem robots) =
    [[posToChar (x, y) | x <- [0 .. maxX]] | y <- [0 .. maxY]] |> unlines
    where
      -- (xs, ys) = robots map fst |> unzip |> (\(xs, ys) -> (maximum xs, maximum ys)))
      setRobots :: Set.Set (Int, Int)
      setRobots = robots |> map fst |> Set.fromList
      posToChar :: (Int, Int) -> Char
      posToChar pos = if pos `Set.member` setRobots then '*' else ' '

maxX :: Int
maxX = 100
--maxX = 10 :: Int

maxY :: Int
maxY = 102
-- maxY = 6 :: Int

midX :: Int
midX = maxX `div` 2 :: Int

midY :: Int
midY = maxY `div` 2 :: Int

instance L.Problem Problem where
  parse1 = parsePart1
  solve1 = solvePart1
  solve2 = solvePart2

--------------------------------------------------------------------------------

solvePart2 :: Problem -> Answer
solvePart2 (Input input) = solvePart2 $ parsePart1 input
solvePart2 (Problem robots) =
  --filter (\n -> robotsFormTree (map (`moveRobot` n) robots)) [1 .. 10000] |> take 1 |> head |> AnswerInt
  let renders :: [String]
      renders =
        map (\n -> map (`moveRobot` n) robots) ns |>
        map (show . Problem) |> zipWith (\n p -> header n ++ p) ns
   in renders |> unlines |> AnswerTxt
  where
    ns = [0..100] |> map (\x -> 25 + x * 103)
    del = replicate 101 '='
    header :: Int -> String
    header seconds = del ++ "\n" ++ "Seconds: " ++ show seconds ++ "\n" ++ del ++ "\n"

-- robotsFormTree :: [Robot] -> Bool
-- robotsFormTree robots =
--   let positions = map fst robots
--       positionsByY :: Map.Map Int [Int]
--       positionsByY = positions
--         |> distinct
--         |> foldl (\m (x, y) -> Map.insertWith (++) y [x] m) Map.empty
--    in positionsByY |> Map.map (\xs -> sum xs `div` length xs) |> Map.elems |> distinct x |> == 1

--------------------------------------------------------------------------------

solvePart1 :: Problem -> Answer
solvePart1 (Input input) = solvePart1 $ parsePart1 input
solvePart1 (Problem robots) =
  let new_robots = map (`moveRobot` 100) robots
      up_lt = filter (\((x, y), _) -> x < midX && y < midY) new_robots |> length
      up_rt = filter (\((x, y), _) -> x > midX && y < midY) new_robots |> length
      dn_lt = filter (\((x, y), _) -> x < midX && y > midY) new_robots |> length
      dn_rt = filter (\((x, y), _) -> x > midX && y > midY) new_robots |> length
   in AnswerInt (up_lt * up_rt * dn_lt * dn_rt)

moveRobot :: Robot -> Int -> Robot
moveRobot r n =
  let ((x, y), (vx, vy)) = r
      new_x = (x + vx * n) `mod` (maxX + 1)
      new_y = (y + vy * n) `mod` (maxY + 1)
   in ((new_x, new_y), (vx, vy))

--------------------------------------------------------------------------------

parsePart1 :: String -> Problem
parsePart1 input = input |> lines |> map parseLine |> Problem

parseLine :: String -> Robot
parseLine line =
  let loc_vel_str = map (drop 2) (words line)
      (loc_str, vel_str) = (head loc_vel_str, loc_vel_str !! 1)
      loc = splitStringAtChar loc_str ',' |> (\(x, y) -> (read x :: Int, read y :: Int))
      vel = splitStringAtChar vel_str ',' |> (\(x, y) -> (read x :: Int, read y :: Int))
   in (loc, vel)

--------------------------------------------------------------------------------
