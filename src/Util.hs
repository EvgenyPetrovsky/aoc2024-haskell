module Util where

-- import Data.Map as Map


greet :: String -> String
greet name = "Hello, " ++ name ++ "!"


(|>) :: a -> (a -> b) -> b
a |> f = f a
infixl 9 |>

stringToInts :: String -> [Int]
stringToInts s = words s |> map read

-- frequency :: (Ord a) => [a] -> Map.Map a Int
-- frequency = foldl emptyM update
--     where
--       emptyM = empty :: Map.Map a Int
--       update z x = Map.insertWith' (+) x 1 z