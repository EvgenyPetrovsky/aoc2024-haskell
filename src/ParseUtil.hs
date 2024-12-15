module ParseUtil
  ( stringToInts,
    inputToLocationCharTupples,
    splitStringAtChar
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

splitStringAtChar :: String -> Char -> (String,String)
splitStringAtChar s c =
  let (p1, p2) = span (/= c) s
  in (p1, tail p2)
