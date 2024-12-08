module Main (main) where

import Lib
import qualified Day08 as D

--import Lib

import Util

main :: IO ()
main = do
    -- putStrLn $ greet "World"

    input <- getContents
    let problem = D.Input input :: D.Problem
    -- let answer1 = solve1 problem

    -- let problem = parse1 input :: Problem
    -- let part1s = solve1 problem
    -- let part2s = solve2 problem

    putStrLn $ "Part 1: " ++ (problem |> solveShow1)
    putStrLn $ "Part 2: " ++ (problem |> solveShow2)
