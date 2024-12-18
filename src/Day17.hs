{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day17
  ( Problem (..),
    Answer (..),
  )
where

import Data.List (intersperse)
import GHC.Bits (xor)
import Lib (Answer (..))
import qualified Lib as L (Problem (..))
import Util

data Computer = Computer
  { regA :: Int,
    regB :: Int,
    regC :: Int,
    program :: [Int],
    pointer :: Int,
    output :: [Int]
  }
  deriving (Show)

data Problem = Input String | Problem Computer

instance L.Problem Problem where
  parse1 = parsePart1
  solve1 = solvePart1
  solve2 = solvePart2

--------------------------------------------------------------------------------

solvePart2 :: Problem -> Answer
solvePart2 (Input input) = solvePart2 $ parsePart1 input
solvePart2 (Problem comp) =
  iterate 0 1 |> AnswerInt
  where
    rev_prg = program comp |> reverse
    total_len = length rev_prg
    iterate :: Int -> Int -> Int
    iterate fix_regA len_to_match =
      let new_out = comp {regA = fix_regA} |> run |> output
          prg_to_match = take len_to_match rev_prg
       in if len_to_match > total_len
          then error "we are trying to match output longer than original program"
          else if new_out == prg_to_match && len_to_match == total_len
          then fix_regA
          else if new_out == prg_to_match && len_to_match < total_len
          then iterate (fix_regA * 8) (len_to_match + 1)
          else iterate (fix_regA + 1) len_to_match

--------------------------------------------------------------------------------

solvePart1 :: Problem -> Answer
solvePart1 (Input input) = solvePart1 $ parsePart1 input
solvePart1 (Problem comp) =
  let terminal_output = run comp |> output |> reverse
   in terminal_output |> map show |> intersperse "," |> concat |> AnswerStr

run :: Computer -> Computer
run comp =
  if is_halt
    then comp
    else
      comp {pointer = new_ipointer}
        |> interpret opcode operand
        |> run
  where
    -- \|> (\new_comp -> trace (show new_comp) run new_comp)

    ipointer = pointer comp
    is_halt = ipointer >= length (program comp)
    new_ipointer = ipointer + 2
    opcode = program comp !! ipointer
    operand = program comp !! (ipointer + 1)

liter :: Int -> Int
liter = id

combo :: Int -> Computer -> Int
combo operand comp
  | operand <= 3 = operand
  | operand == 4 = regA comp
  | operand == 5 = regB comp
  | operand == 6 = regC comp
  | otherwise = error $ "invalid value of combo operand: " ++ show operand

interpret :: Int -> Int -> Computer -> Computer
interpret opcode operand comp
  | opcode == 0 = let result = regAval `div` (2 ^ cmbVal) in comp {regA = result}
  | opcode == 1 = let result = regBval `xor` litVal in comp {regB = result}
  | opcode == 2 = let result = cmbVal `mod` 8 in comp {regB = result}
  | opcode == 3 = if regAval == 0 then comp else comp {pointer = litVal}
  | opcode == 4 = let result = regBval `xor` regCval in comp {regB = result}
  | opcode == 5 = let result = cmbVal `mod` 8 in comp {output = result:out}
  | opcode == 6 = let result = regAval `div` (2 ^ cmbVal) in comp {regB = result}
  | opcode == 7 = let result = regAval `div` (2 ^ cmbVal) in comp {regC = result}
  | otherwise = error $ "unknown opcode: " ++ show opcode
  where
    regAval = regA comp :: Int
    regBval = regB comp :: Int
    regCval = regC comp :: Int
    cmbVal = combo operand comp :: Int
    litVal = liter operand :: Int
    out = output comp

--------------------------------------------------------------------------------

parsePart1 :: String -> Problem
parsePart1 input =
  let all_lines = lines input
      registers :: [Int]
      registers = take 3 all_lines |> map (read . drop 12)
      op_codes :: [Int]
      op_codes =
        all_lines
          !! 4
          |> drop 9
          |> map (\c -> if c == ',' then ' ' else c)
          |> words
          |> map read
   in Problem
        Computer
          { regA = head registers,
            regB = registers !! 1,
            regC = registers !! 2,
            program = op_codes,
            pointer = 0,
            output = []
          }

--------------------------------------------------------------------------------
