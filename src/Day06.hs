module Day06
    ( Problem (..)
    , Answer (..)
    ) where

import qualified Lib as L ( Problem (..) )
import Lib ( Answer (..) )
import Util
import Data.List (elemIndex)

type Position = (Int,Int)
type Obstacle = Position
data Direction = N | E | S | W
instance Enum Direction where
    fromEnum x = case x of
        N -> 0
        E -> 1
        S -> 2
        W -> 3
    toEnum x = case x `mod` 4 of
        0 -> N
        1 -> E
        2 -> S
        3 -> W
        _ -> error "incomplete definition of Enum instance for Direction"

data Guard = Guard {pos :: Position, dir :: Direction, trace :: [Position]}

-- type Answer = L.Answer
data Definition = Definition {rows :: Int, cols :: Int, obstacles :: [Obstacle], guard :: Guard}
data Problem = Input String | Problem Definition

instance L.Problem Problem where
    parse1 = parsePart1

    solve1 = solvePart1
    solve2 _ = Unknown

--------------------------------------------------------------------------------

solvePart1 :: Problem -> Answer
solvePart1 (Problem definition) =
    let g = guard definition
        o = obstacles definition
    in moveGuardOut g o |> trace |> deduplicate |> length |> AnswerInt
    where
        moveGuardOut :: Guard -> [Obstacle] -> Guard
        moveGuardOut g o =
            let new_g = moveGuard g o
            in if pos new_g `insideMap` definition then moveGuard new_g o else new_g
        deduplicate :: Eq a => [a] -> [a]
        deduplicate [] = []
        --deduplicate [x] = [x]
        deduplicate (x:xs) =
            if x `notElem` xs then x : deduplicate xs
            else deduplicate xs
solvePart1 (Input input) = solvePart1 $ L.parse1 input
--nextDirection :

parsePart1 :: String -> Problem
parsePart1 s =
    let lns = lines s
        rows' = length lns
        cols' = length $ head lns
        rc = [(r,c) | r <- [1..rows'], c <- [1..cols']]
        guard_pos = filter (\(r,c) -> isGuard (lns!!(r-1)!!(c-1))) rc |> head
        guard_dir = guard_pos |> (\(r,c) -> toDirection (lns!!(r-1)!!(c-1)))
        guard' = Guard guard_pos guard_dir []
        obstacles' = filter (\(r,c) -> '#' == lns!!(r-1)!!(c-1)) rc
        definition = Definition rows' cols' obstacles' guard'
    in
        Problem definition
    where
        isGuard :: Char -> Bool
        isGuard c = c `elem` "^>v<"
        toDirection :: Char -> Direction
        toDirection c = case c `elemIndex` "^>v<" of
            Just idx -> toEnum idx :: Direction
            Nothing  -> error $ "Wrong direction character: " ++ [c]

--------------------------------------------------------------------------------

nextPosition :: Position -> Direction -> Position
nextPosition (r,c) d =
    case d of
        N -> (r-1,c)
        E -> (r,c+1)
        S -> (r+1,c)
        W -> (r,c-1)
insideMap :: Position -> Definition -> Bool
insideMap (r, c) definition =
    0 < r && r <= rows definition && 0 < c && c <= cols definition

isObstacle :: Position -> [Obstacle] -> Bool
isObstacle = elem

moveGuard :: Guard -> [Obstacle] -> Guard
moveGuard g obs =
    let new = nextPosition p d
    in
        if not (isObstacle new obs) then Guard {pos = new, dir = d, trace = p:t}
        else moveGuard (Guard {pos = p, dir = succ d, trace = t}) obs
    where
        Guard {pos = p, dir = d, trace = t} = g