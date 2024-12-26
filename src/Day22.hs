{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day22
  ( Problem (..),
    Answer (..),
  )
where

import Lib (Answer (..))
import qualified Lib as L (Problem (..))
import Util ( (|>) )
import GHC.Bits (Bits(xor))
import qualified Data.Map.Strict as Map
import Data.List (zip5)

--type SequencesPrices = Map.Map (Int,Int,Int,Int) Int

data Problem = Input String | Problem [Int]

instance L.Problem Problem where
  parse1 = parsePart1
  solve1 = solvePart1
  solve2 = solvePart2

--------------------------------------------------------------------------------

solvePart2 :: Problem -> Answer
solvePart2 (Input input) = solvePart2 $ parsePart1 input
solvePart2 (Problem codes) =
  codes
    |> map (map price . take 2001 . iterate next)
    |> concatMap seqScores
    |> foldl (\z (seq, price) -> Map.insertWith (+) seq price z) Map.empty
    |> Map.elems
    |> maximum
    |> AnswerInt
  -- where
  --   generate2000 :: Int -> [Int]
  --   generate2000 start = take 2001 $ iterate next start

price :: Int -> Int
price a = a `mod` 10

seqScores :: [Int] -> [((Int,Int,Int,Int), Int)]
seqScores prices =
  let
    scores = map (\(p0,p1,p2,p3,p4) -> ((p1-p0,p2-p1,p3-p2,p4-p3),p4)) $ zip5 p0 p1 p2 p3 p4
    scoremap = foldl (\z (seq, val) -> Map.insertWith (\_ b -> b) seq val z) Map.empty scores
    scores_dedup = Map.toList scoremap
  in scores_dedup
  where
  p0 = prices
  p1 = tail p0
  p2 = tail p1
  p3 = tail p2
  p4 = tail p3
--------------------------------------------------------------------------------

solvePart1 :: Problem -> Answer
solvePart1 (Input input) = solvePart1 $ parsePart1 input
solvePart1 (Problem codes) =
  codes |> map (`simulate` 2000) |> sum |> AnswerInt

simulate :: Int -> Int -> Int
simulate start times = iterate next start !! times

next :: Int -> Int
next secret =
  secret |> step1 |> step2 |> step3
  where
    step1 secret = prune . mixin secret $ secret * 64
    step2 secret = prune . mixin secret $ div secret 32
    step3 secret = prune . mixin secret $ secret * 2048
    mixin = xor
    prune :: Int -> Int
    prune x = x `mod` 16777216

--------------------------------------------------------------------------------

parsePart1 :: String -> Problem
parsePart1 input = input |> lines |> map read |> Problem

--------------------------------------------------------------------------------
