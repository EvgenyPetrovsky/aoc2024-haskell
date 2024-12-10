{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day07
  ( Problem (..),
    Answer (..),
  )
where

import Lib (Answer (..))
import qualified Lib as L (Problem (..))
import Util

data Op = Add | Mul | Con

-- type Answer = L.Answer
data Definition
  = Line
  { result :: Int,
    operands :: [Int]
  }
  deriving (Show)

data Problem = Input String | Problem [Definition] deriving (Show)

instance L.Problem Problem where
  parse1 = parsePart1
  solve1 = solvePart1
  solve2 = solvePart2

--------------------------------------------------------------------------------

solvePart2 :: Problem -> Answer
solvePart2 (Input input) = solvePart2 . L.parse2 $ input
solvePart2 (Problem lines) =
  let op_set = [Add, Mul, Con] :: [Op]
   in lines
        |> filter (\l -> isValidLine (result l) (operands l) op_set)
        |> map result
        |> sum
        |> AnswerInt

parsePart1 :: String -> Problem
parsePart1 input =
  input |> lines |> map (\l -> Line (parse_res l) (parse_nms l)) |> Problem
  where
    parse_res :: String -> Int
    parse_res l = l |> takeWhile (/= ':') |> read :: Int
    parse_nms :: String -> [Int]
    parse_nms l = l |> words |> tail |> map read :: [Int]

solvePart1 :: Problem -> Answer
solvePart1 (Input input) = solvePart1 . L.parse1 $ input
solvePart1 (Problem lines) =
  let op_set = [Add, Mul] :: [Op]
   in lines
        |> filter (\l -> isValidLine (result l) (operands l) op_set)
        |> map result
        |> sum
        |> AnswerInt

isValidLine :: Int -> [Int] -> [Op] -> Bool
isValidLine given_final_val number_list op_set =
  let (head_number, tail_numbers) = splitAt 1 number_list
      final_vals = foldl (\z x -> step op_set z x |> filter (<= given_final_val)) head_number tail_numbers
   in given_final_val `elem` final_vals

step :: [Op] -> [Int] -> Int -> [Int]
step ops in_vals val =
  [evaluate op in_val val | op <- ops, in_val <- in_vals]

evaluate :: Op -> Int -> Int -> Int
evaluate Add x y = x + y
evaluate Mul x y = x * y
evaluate Con x y = read (show x ++ show y)