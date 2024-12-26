{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day24
  ( Problem (..),
    Answer (..),
  )
where

import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import GHC.Bits (Bits (xor, (.&.), (.|.)))
import Lib (Answer (..))
import qualified Lib as L (Problem (..))
import Util ((|>))

-- data Signal = S_ | S0 | S1 deriving (Show)
type Signal = Maybe Int

type WireId = String

type Wire = (WireId, Signal)

type State = Map.Map WireId Signal

data Op = XOR | AND | OR deriving (Show, Eq)

data Gate = Gate {in1 :: WireId, in2 :: WireId, op :: Op, out :: WireId} deriving (Show)

type Definition = (State, [Gate])

data Problem = Input String | Problem Definition

instance L.Problem Problem where
  parse1 = parsePart1
  solve1 = solvePart1
  solve2 = solvePart2

--------------------------------------------------------------------------------

solvePart2 :: Problem -> Answer
solvePart2 (Input input) = solvePart2 $ parsePart1 input
solvePart2 (Problem (state, gates)) =
  let final_state = iter state gates
  in AnswerTxt $
      -- "Gates: \n  " ++ show gates ++ "\n" ++
      -- "Initial State: \n  " ++ show state ++ "\n" ++
      -- "Final State: \n  " ++ show final_state ++ "\n" ++
      "X-wires: " ++ show (extractSignal final_state 'x') ++ "\n" ++
      "Y-wires: " ++ show (extractSignal final_state 'y') ++ "\n" ++
      "Z-wires: " ++ show (extractSignal final_state 'z') ++ "\n"

extractSignal :: State -> Char -> [Int]
extractSignal state c =
  state
  |> Map.toList
  |> mapMaybe (\(wire_id, signal) -> if c == head wire_id then signal else Nothing)
  |> reverse

--------------------------------------------------------------------------------

solvePart1 :: Problem -> Answer
solvePart1 (Input input) = solvePart1 $ parsePart1 input
solvePart1 (Problem (state, gates)) =
  let final_state = iter state gates
   in final_state |> extractAnswer |> AnswerInt

--  in AnswerTxt $
--       "Gates: \n  " ++ show gates ++ "\n" ++
--       "Initial State: \n  " ++ show state ++ "\n" ++
--       "Final State: \n  " ++ show final_state

iter :: State -> [Gate] -> State
iter state gates =
  let gates_to_process = filter ready gates
      new_state = foldl gateOp state gates_to_process
   in if null gates_to_process then state else iter new_state gates
  where
    ready :: Gate -> Bool
    ready g = case (state Map.! in1 g, state Map.! in2 g, state Map.! out g) of
      (Just _, Just _, Nothing) -> True
      _ -> False

gateOp :: State -> Gate -> State
gateOp state Gate {op = gop, in1 = gi1, in2 = gi2, out = gout} =
  Map.insert gout (apply gop (state Map.! gi1) (state Map.! gi2)) state
  where
    apply :: Op -> Signal -> Signal -> Signal
    apply AND = liftA2 (.&.)
    apply OR = liftA2 (.|.)
    apply XOR = liftA2 xor

extractAnswer :: State -> Int
extractAnswer state =
  state
    |> Map.toList
    |> mapMaybe (\(wire_id, signal) -> if "z" `isPrefixOf` wire_id then signal else Nothing)
    |> reverse
    |> foldl (\z x -> z * 2 + x) 0

--------------------------------------------------------------------------------
parsePart1 :: String -> Problem
parsePart1 input =
  let all_lines = lines input
      (wires_txt, gates_txt) = break (== "") all_lines |> (\(a, b) -> (a, tail b))
      wires = map parseWire wires_txt
      gates = map parseGate gates_txt
   in (initState wires gates, gates) |> Problem

parseWire :: String -> Wire
parseWire s =
  let (name, val) = s |> filter (/= ':') |> words |> (\ws -> (head ws, ws !! 1))
      signal = case val of
        "0" -> Just 0
        "1" -> Just 1
        x -> error $ "can't parse wire with signal " ++ x
   in (name, signal)

parseGate :: String -> Gate
parseGate s =
  let ws = s |> filter (`notElem` "->") |> words
   in Gate
        { in1 = head ws,
          op = parseOp (ws !! 1),
          in2 = ws !! 2,
          out = ws !! 3
        }

parseOp :: String -> Op
parseOp "AND" = AND
parseOp "OR" = OR
parseOp "XOR" = XOR
parseOp x = error $ "unknown operation: " ++ x

initState :: [Wire] -> [Gate] -> State
initState wires gates =
  let state0 = gates |> foldl (\s g -> foldl (\s1 g1 -> Map.insert g1 Nothing s1) s [in1 g, in2 g, out g]) Map.empty
   in foldl (\m (wid, wsig) -> Map.insert wid wsig m) state0 wires

--------------------------------------------------------------------------------
