module Lib
    ( Problem (..)
    , Answer (..)
    ) where

data Answer = Unknown | AnswerInt Int | AnswerStr String | AnswerTxt String
instance Show Answer where
    show Unknown = "not known yet"
    show (AnswerInt n) = show n
    show (AnswerStr s) = s
    show (AnswerTxt t) = "\n" ++ t

class Problem p where
    parse1 :: String -> p
    parse2 :: String -> p
    parse2 = parse1
    solve1 :: p -> Answer
    solve2 :: p -> Answer
    solveShow1 :: p -> String
    solveShow1 = show . solve1
    solveShow2 :: p -> String
    solveShow2 = show . solve2