module Util
  ( (|>),
    stringToInts,
    frequency,
    distinct,
    digitCount,
    locationOrtoNeighbours2d,
    filterNot
  )
where

import qualified Data.Map.Strict as Map (Map, empty, insertWith)
--import qualified Data.Set as Set (fromList, toList)
import Data.List (nub)

(|>) :: a -> (a -> b) -> b
a |> f = f a

infixl 9 |>

stringToInts :: String -> [Int]
stringToInts s = words s |> map read

frequency :: (Ord a) => [a] -> Map.Map a Int
frequency = foldl update emptyM
  where
    emptyM = Map.empty :: Map.Map a Int
    update z x = Map.insertWith (+) x 1 z

distinct :: (Ord a) => [a] -> [a]
distinct a = a |> nub

digitCount :: Int -> Int
digitCount 0 = 1
digitCount n =
  let abs_n = abs n :: Int
      numbers = scanl1 (\z _ -> z * 10) [1 .. 18 :: Int]
   in takeWhile (<= abs_n) numbers |> length

-- filterMap :: Foldable m => (a -> Maybe b) -> m a -> m b
-- filterMap f ma = map f ma |> filter'
--  where filter' (empty ma) = empty

locationOrtoNeighbours2d :: (Int, Int) -> [(Int, Int)]
locationOrtoNeighbours2d (x,y) =
    [(x + dx, y + dy) | (dx, dy) <- [(0,-1),(1,0),(0,1),(-1,0)]]

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = filter (not . f)