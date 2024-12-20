{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day19
  ( Problem (..),
    Answer (..),
  )
where

import Algorithm.Search (dijkstraAssoc, pruningAssoc)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)
import Lib (Answer (..))
import qualified Lib as L (Problem (..))
import Util (filterNot, (|>))

type Cost = Int

type State = String

type Cache = Map.Map String Int

data Definition = Definition
  { towels :: [String],
    patterns :: [String]
  }
  deriving (Show)

data Problem = Input String | Problem Definition

instance Show Problem where
  show (Input input) = "Unparsed input: " ++ input
  show (Problem definition) = show definition

instance L.Problem Problem where
  parse1 = parsePart1
  solve1 = solvePart1
  solve2 = solvePart2

--------------------------------------------------------------------------------

solvePart2 :: Problem -> Answer
solvePart2 (Input input) = solvePart2 $ parsePart1 input
solvePart2 (Problem definition) =
  let ts = towels definition
      ps = patterns definition
   in ps |> map (customSearch ts) |> sum |> AnswerInt

customSearch :: [String] -> String -> Int
customSearch towels pattern =
  let (options, _) = iterate pattern (Map.empty :: Cache)
  in
  trace
    ("processing pattern " ++ show pattern ++ " with result: " ++ show options)
    options
  where
    iterate :: String -> Cache -> (Int, Cache)
    iterate remp cache =
      case Map.lookup remp cache of
        Just cache_val -> (cache_val, cache)
        Nothing ->
          let (tot_opts, new_cache) = foldl foldWithCache (0, cache) matching_ts
          in (tot_opts, Map.insert remp tot_opts new_cache)
      where
        matching_ts = filter (`isPrefixOf` remp) towels
        foldWithCache (acc_opt, acc_cache) matching_t =
          let new_remp = drop (length matching_t) remp
              (out_opt, out_cache) = iterate new_remp acc_cache
            in
              if matching_t == remp
              then (acc_opt + 1, Map.insert remp 1 cache)
              else (acc_opt + out_opt, out_cache)

--------------------------------------------------------------------------------

solvePart1 :: Problem -> Answer
solvePart1 (Input input) = solvePart1 $ parsePart1 input
solvePart1 (Problem definition) =
  -- AnswerTxt $ show definition
  let ts = towels definition
      ps = patterns definition
   in ps |> mapMaybe (solve ts) |> length |> AnswerInt

solve :: [String] -> String -> Maybe (Cost, [String])
solve towels pattern =
  dijkstraAssoc (next `pruningAssoc` (\(s, _) -> length s > length pattern)) found []
  where
    fixed_cost = 1 :: Cost
    found :: State -> Bool
    found = (== pattern)
    next :: State -> [(State, Cost)]
    next s =
      towels
        |> map (s ++)
        |> filter (and . zipWith (==) pattern)
        |> map (\s -> (s, fixed_cost))

--------------------------------------------------------------------------------

parsePart1 :: String -> Problem
parsePart1 input =
  let all_lines = lines input
      towels = head all_lines |> filterNot (== ',') |> words
      patterns = drop 2 all_lines
   in Definition towels patterns |> Problem