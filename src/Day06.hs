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
data Direction = N | E | S | W deriving (Show, Eq)
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

data Guard = 
    Guard {
        position :: Position, 
        direction :: Direction, 
        trace :: [(Position,Direction)]
    } deriving (Show)

data Field = 
    Field {
        rows :: Int, columns :: Int, 
        obstacles :: [Obstacle]
    } deriving (Show)

-- type Answer = L.Answer
data Definition = 
    Definition {
        field :: Field, 
        guard :: Guard
    } deriving (Show)

data Problem = Input String | Problem Definition deriving (Show)

instance L.Problem Problem where
    parse1 = parsePart1
    solve1 = solvePart1
    solve2 = solvePart2

--------------------------------------------------------------------------------

-- solvePart2 :: Problem -> Answer
-- solvePart2 (Problem d) =
--     let f = d |> field
--         obs = f |> obstacles
--         pos = d |> guard |> position
--         rows' = rows f
--         cols' = columns f
--         ps = [(r,c) | r <- [1..rows'], c <- [1..cols'], (r,c) `notElem` obs, (r,c) /= pos] 
--         ds = [N,E,S,W] :: [Direction]
--     in filter (\p' -> any (\d' -> checkObstaclePlacement p' d' obs) ds) ps |> length |> AnswerInt

solvePart2 :: Problem -> Answer
solvePart2 (Problem d) =
    let g = guard d
        f = field d
        original_obstacles = f |> obstacles
        obstacle_options = 
            g `moveGuardOut` f |> trace |> map fst |> deduplicate |> filter (\rc -> rc /= position g)
    in obstacle_options |> filter (\o -> g `loopGuardIn` f {obstacles = o:original_obstacles}) |> length |> AnswerInt
    
solvePart2 (Input input) = solvePart2 $ L.parse1 input

solvePart1 :: Problem -> Answer
solvePart1 (Problem d) =
    let g = guard d
        f = field d
    in moveGuardOut g f |> trace |> map fst |> deduplicate |> length |> AnswerInt

solvePart1 (Input input) = solvePart1 $ L.parse1 input
--nextDirection :

deduplicate :: Eq a => [a] -> [a]
deduplicate [] = []
deduplicate (x:xs) =
    if x `notElem` xs then x : deduplicate xs
    else deduplicate xs

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
        field' = Field rows' cols' obstacles'
        definition = Definition field' guard'
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

moveGuardOut :: Guard -> Field -> Guard
moveGuardOut g f =
    let o = obstacles f
        new_g = moveGuard g o
    in if new_g `insideMap` f then new_g `moveGuardOut` f else new_g

loopGuardIn :: Guard -> Field -> Bool
loopGuardIn g f =
    let o = obstacles f
        new_g = moveGuard g o
    in insideLoop new_g || (new_g `insideMap` f) && (new_g `loopGuardIn` f)

nextPosition :: Position -> Direction -> Position
nextPosition (r,c) d =
    case d of
        N -> (r-1,c)
        E -> (r,c+1)
        S -> (r+1,c)
        W -> (r,c-1)

-- prevPosition :: Position -> Direction -> Position
-- prevPosition p d = nextPosition p (succ . succ $ d) 

insideMap :: Guard -> Field -> Bool
insideMap g f =
    let (r,c) = position g
        rmax = rows f
        cmax = columns f
    in 0 < r && r <= rmax && 0 < c && c <= cmax

insideLoop :: Guard -> Bool
insideLoop g =
    let t = g |> trace
    in head t `elem` tail t

isObstacle :: Position -> [Obstacle] -> Bool
isObstacle = elem

moveGuard :: Guard -> [Obstacle] -> Guard
moveGuard g obs =
    let new = nextPosition p d
    in
        if not (isObstacle new obs) then Guard {position = new, direction = d, trace = (p,d):t}
        else moveGuard (Guard {position = p, direction = succ d, trace = t}) obs
    where
        Guard {position = p, direction = d, trace = t} = g


-- checkObstaclePlacement :: Position -> Direction -> [Obstacle] -> Bool
-- checkObstaclePlacement p d org_obs =
--     let new_obs = p:org_obs
--         dir0 = d
--         obs0 = Just p
--         prec_p0 = fmap (`prevPosition` dir0) obs0
--         dir1 = succ dir0
--         obs1 = prec_p0 >>= (\p' -> nextObstacle p' dir1 new_obs)
--         prec_p1 = obs1 |> fmap (`prevPosition` dir1)
--         dir2 = succ dir1
--         obs2 = prec_p1 >>= (\p' -> nextObstacle p' dir2 new_obs)
--         prec_p2 = obs2 |> fmap (`prevPosition` dir2)
--         dir3 = succ dir2
--         obs3 = prec_p2 >>= (\p' -> nextObstacle p' dir3 new_obs)
--         prec_p3 = obs3 |> fmap (`prevPosition` dir3)
--         dir4 = succ dir3
--         obs4 = prec_p3 >>= (\p' -> nextObstacle p' dir4 new_obs)
--     in (case (obs0, obs4) of 
--             (Just a, Just b) -> a == b
--             _ -> False)

-- nextObstacle :: Position -> Direction -> [Obstacle] -> Maybe Obstacle
-- nextObstacle p d os =
--     let (pr, pc) = p
--         on_way = os |> 
--             filter (\(obr,obc) -> case d of 
--                 N -> obc == pc && obr < pr
--                 E -> obr == pr && obc > pc
--                 S -> obc == pc && obr > pr
--                 W -> obr == pr && obc < pc)
--     in case (d, on_way) of
--         (_, []) -> Nothing
--         (N, _) -> Just $ maximum on_way
--         (E, _) -> Just $ minimum on_way
--         (S, _) -> Just $ minimum on_way
--         (W, _) -> Just $ maximum on_way
