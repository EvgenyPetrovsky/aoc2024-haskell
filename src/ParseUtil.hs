module ParseUtil
  ( stringToInts,
    inputToLocationCharTupples,
  )
where

import Util ((|>))

type Location2D = (Int, Int)

stringToInts :: String -> [Int]
stringToInts s = words s |> map read

inputToLocationCharTupples :: String -> [(Location2D, Char)]
inputToLocationCharTupples input =
  [ ((row, col), chr)
    | (row, ln) <- zip [1 ..] (lines input),
      (col, chr) <- zip [1 ..] ln
  ]
