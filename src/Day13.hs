{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day13
  ( Problem (..),
    Answer (..),
  )
where

-- import Text.Parsec.Error (ParseError)

import Control.Applicative ((<|>))
import Data.Maybe (mapMaybe)
import Lib (Answer (..))
import qualified Lib as L (Problem (..))
import Text.Parsec (parse)
import Text.Parsec.Char (newline, string)
import Text.Parsec.Combinator (optional, sepBy1)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Number (int)
import Util

data Machine = Machine
  { a :: (Int, Int),
    b :: (Int, Int),
    prize :: (Int, Int)
  }
  deriving (Show)

data Problem = Input String | Problem [Machine] deriving (Show)

instance L.Problem Problem where
  parse1 = parsePart1
  solve1 = solvePart1
  solve2 = solvePart2

--------------------------------------------------------------------------------

solvePart2 :: Problem -> Answer
solvePart2 (Input input) = solvePart2 $ parsePart1 input
solvePart2 (Problem original_machines) =
  machines
    |> map (\m -> if collinear (a m) (b m) then error "" else m)
    |> mapMaybe (\m -> analyticSolution (a m) (b m) (prize m))
    |> map (uncurry cost)
    |> sum
    |> AnswerInt
  where
    machines = map update original_machines
    addon = 10000000000000
    update :: Machine -> Machine
    update m = let (xp, yp) = prize m in m {prize = (xp + addon, yp + addon)}

--------------------------------------------------------------------------------

solvePart1 :: Problem -> Answer
solvePart1 (Input input) = solvePart1 $ parsePart1 input
solvePart1 (Problem machines) =
  machines
    |> map (\m -> if collinear (a m) (b m) then error "" else m)
    |> mapMaybe (\m -> analyticSolution (a m) (b m) (prize m))
    |> map (uncurry cost)
    |> sum
    |> AnswerInt

analyticSolution :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
analyticSolution (xa, ya) (xb, yb) (xp, yp) =
  if mod_a /= 0 || mod_b /= 0 then Nothing else Just (a, b)
  where
    (b, mod_b) = (xp * ya - yp * xa) `divMod` (xb * ya - xa * yb)
    (a, mod_a) = (xp - b * xb) `divMod` xa

collinear :: (Int, Int) -> (Int, Int) -> Bool
collinear (xa, ya) (xb, yb) = xa * yb == xb * ya

cost :: Int -> Int -> Int
cost ma mb = ma * 3 + mb

--------------------------------------------------------------------------------

parsePart1 :: String -> Problem
parsePart1 input = do
  case parse parseMachines "" input of
    Left err -> error $ show err
    Right machines -> Problem machines

parseCoordinate :: Parser (Int, Int)
parseCoordinate = do
  _ <- string "X"
  _ <- optional (string "=" <|> string "+")
  x <- int
  _ <- string ", "
  _ <- string "Y"
  _ <- optional (string "=" <|> string "+")
  y <- int
  return (x, y)

parseBlock :: String -> Parser (Int, Int)
parseBlock label = do
  _ <- string label
  _ <- string ": "
  parseCoordinate

parseMachine :: Parser Machine
parseMachine = do
  aCoords <- parseBlock "Button A"
  _ <- newline
  bCoords <- parseBlock "Button B"
  _ <- newline
  prizeCoords <- parseBlock "Prize"
  return Machine {a = aCoords, b = bCoords, prize = prizeCoords}

parseMachines :: Parser [Machine]
parseMachines = parseMachine `sepBy1` (newline >> newline)
