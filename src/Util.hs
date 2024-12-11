module Util
  ( (|>),
    stringToInts,
    frequency,
    distinct,
    digitCount,
  )
where

import qualified Data.Map.Strict as Map (Map, empty, insertWith)
import qualified Data.Set as Set (fromList, toList)

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
distinct a = a |> Set.fromList |> Set.toList

digitCount :: Int -> Int
digitCount 0 = 1
digitCount n =
  let abs_n = abs n :: Int
      numbers = scanl1 (\z _ -> z * 10) [1 .. 18 :: Int]
   in takeWhile (<= abs_n) numbers |> length
