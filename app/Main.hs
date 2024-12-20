module Main (main) where

import qualified Day20 as D
import Lib
import Util

main :: IO ()
main = do

  input <- getContents
  let problem = D.Input input :: D.Problem

  putStrLn $ "Part 1: " ++ (problem |> solveShow1)
  putStrLn $ "Part 2: " ++ (problem |> solveShow2)
