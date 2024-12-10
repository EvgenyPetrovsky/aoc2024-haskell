module Day05
  ( Problem (..),
    Answer (..),
    rulesToSeq,
    middlePage,
  )
where

import Data.List (elemIndex)
import Lib (Answer (..))
import qualified Lib as L (Problem (..))
import Util

-- type Answer = L.Answer
type Page = Int

type OrdRule = (Page, Page)

type Manual = [Page]

data Definition = Definition {orders :: [OrdRule], manuals :: [Manual]}

data Problem = Input String | Problem Definition

instance L.Problem Problem where
  parse1 = parseInput
  solve1 = solvePart1
  solve2 = solvePart2

{------------------------------------------------------------------------------}

-- {- Epic (and failed) attempt to recreate manual solely based on rules:
--     rules do not allow to uniquely place all pages:
--     there are clusters unordered clusters
--     rules must only spot violations, but do not define definitive order
-- -}
-- solvePart1 :: Problem -> Answer
-- solvePart1 (Problem {orders = ords, manuals = mans}) =
--     let full_manual = rulesToSeq [] (sort ords)
--     in mans |> filter(`subsetOf` full_manual) |> map middlePage |> sum |> AnswerInt
--     where
--         subsetOf :: Manual -> Manual -> Bool
--         m `subsetOf` full = m == filter (`elem` m) full
solvePart1 :: Problem -> Answer
solvePart1 (Problem definition) =
  filter (`validateWithOrds` ords) mans |> map middlePage |> sum |> AnswerInt
  where
    Definition {orders = ords, manuals = mans} = definition
solvePart1 (Input input) = solvePart1 . parseInput $ input

solvePart2 :: Problem -> Answer
solvePart2 (Problem definition) =
  filter (\m -> not $ m `validateWithOrds` ords) mans
    |> map (`reorderManual` ords)
    |> map middlePage
    |> sum
    |> AnswerInt
  where
    Definition {orders = ords, manuals = mans} = definition
solvePart2 (Input input) = solvePart2 . parseInput $ input

{------------------------------------------------------------------------------}

middlePage :: [Page] -> Page
middlePage ps =
  let half_len = length ps `div` 2
   in ps !! half_len

{- Validate sequence of pages by spliting it into 2 parts with page in between
   and checkwing that rules for this page and pager to the left and right hold -}
validateWithOrds :: Manual -> [OrdRule] -> Bool
validateWithOrds manual ords =
  zipWith (\idx _ -> splitAndCheck idx) [0 ..] manual |> and
  where
    -- ords' p = filter(\(l,r) -> l == p || r == p) ords
    before :: [OrdRule] -> Page -> [Page]
    before o p = filter (\(_, r) -> p == r) o |> map fst
    after :: [OrdRule] -> Page -> [Page]
    after o p = filter (\(l, _) -> p == l) o |> map snd
    splitAndCheck :: Int -> Bool
    splitAndCheck idx = check (take idx manual) (drop (idx + 1) manual) (manual !! idx)
    check :: [Page] -> [Page] -> Page -> Bool
    check l r p =
      (filter (\p' -> p' `elem` after ords p) l ++ filter (\p' -> p' `elem` before ords p) r)
        |> length
        |> (== 0)

{- Reorder the manual based on rules -}
reorderManual :: Manual -> [OrdRule] -> Manual
reorderManual m ords =
  let relevantOrds = filter (\(l, r) -> l `elem` m && r `elem` m) ords
      new_ord = foldl orderPages m relevantOrds
   in if new_ord == m then m else reorderManual new_ord ords
  where
    orderPages in_man (l, r) = case (elemIndex l in_man, elemIndex r in_man) of
      (Just li, Just ri) -> if li < ri then in_man else swapPages in_man l r
      _ -> m

swapPages :: Manual -> Page -> Page -> Manual
swapPages m l r = map (\p -> if p == l then r else if p == r then l else p) m

rulesToSeq :: [Page] -> [OrdRule] -> [Page]
rulesToSeq ps os =
  let newps = foldl addPages ps os
   in if newps == ps then ps else rulesToSeq newps os
  where
    addPages :: [Page] -> OrdRule -> [Page]
    addPages manual (l, r) = case (elemIndex l manual, elemIndex r manual) of
      (Nothing, Nothing) -> l : r : manual
      -- (Nothing, Just ri) -> splitAt ri manual |> (\(p1,p2) -> p1 ++ l:p2)
      -- (Just li, Nothing) -> splitAt (li+1) manual |> (\(p1,p2) -> p1 ++ r:p2)
      (Just li, Just ri) ->
        if li < ri
          then manual
          else addPages (take li manual ++ tail (drop li manual)) (l, r)
      (Nothing, Just _) -> l : manual
      (Just _, Nothing) -> manual ++ [r]

parseInput :: String -> Problem
parseInput input =
  let lns = lines input
      _orders = lns |> takeWhile (/= "") |> map parseOrder
      _manuals = lns |> dropWhile (/= "") |> tail |> map parseManual
   in Problem $ Definition {orders = _orders, manuals = _manuals}
  where
    parseOrder :: String -> OrdRule
    parseOrder o =
      o
        |> map (\x -> if x == '|' then ' ' else x)
        |> words
        |> map read
        |> (\x -> (head x, x !! 1)) -- (\(x1:x2:_) -> (x1, x2))
    parseManual :: String -> Manual
    parseManual m =
      m
        |> map (\x -> if x == ',' then ' ' else x)
        |> words
        |> map read
