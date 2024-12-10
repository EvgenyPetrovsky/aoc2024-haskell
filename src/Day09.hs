{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day09
  ( Problem (..),
    Answer (..),
  )
where

import Data.Char (digitToInt)
import Lib (Answer (..))
import qualified Lib as L (Problem (..))
import Util

data Block = E | B Int deriving (Show)

type Len = Int

type Idx = Int

data Block2 = E2 Len | B2 Len Idx deriving (Show)

data Problem = Input String | Problem [Block] | Problem2 [Block2]

instance Show Problem where
  show (Input s) = show s
  show (Problem p) = map blockToChar p
    where
      blockToChar :: Block -> Char
      blockToChar E = '.'
      blockToChar (B idx) = ids !! idx
      ids :: String
      ids =
        [ c | _ <- [1 ..] :: [Int], c <- ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']
        ]
  show (Problem2 p) = concatMap blockToStr p
    where
      blockToStr :: Block2 -> String
      blockToStr (E2 len) = replicate len '.'
      blockToStr (B2 len idx) = replicate len (ids !! idx)
      ids :: String
      ids =
        [ c | _ <- [1 ..] :: [Int], c <- ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']
        ]

instance L.Problem Problem where
  parse1 = parsePart1
  parse2 = parsePart2
  solve1 = solvePart1
  solve2 = solvePart2

--------------------------------------------------------------------------------

solvePart2 :: Problem -> Answer
solvePart2 (Input input) = solvePart2 $ parsePart2 input
solvePart2 (Problem _) = error "Part 2 uses Problem1 format"
solvePart2 (Problem2 blocks) =
  blocks |> reorder2 |> checkSum2 |> AnswerInt

reorder2 :: [Block2] -> [Block2]
reorder2 blocks =
  let revblocks = reverse blocks
   in foldl (flip tryMoveBlock) blocks revblocks

tryMoveBlock :: Block2 -> [Block2] -> [Block2]
tryMoveBlock _ [] = []
tryMoveBlock (E2 _) blocks = blocks
tryMoveBlock (B2 lenToMove idxToMove) (block : rem_blocks) =
  let oneToMove = B2 lenToMove idxToMove
   in case block of
        E2 space ->
          if space < lenToMove
            then block : tryMoveBlock oneToMove rem_blocks
            else
              if space == lenToMove
                then oneToMove : cleanBlock idxToMove rem_blocks
                else oneToMove : E2 (space - lenToMove) : cleanBlock idxToMove rem_blocks
        B2 _ idx ->
          if idx == idxToMove
            then block : rem_blocks
            else block : tryMoveBlock oneToMove rem_blocks

cleanBlock :: Int -> [Block2] -> [Block2]
cleanBlock _ [] = []
cleanBlock idx ((B2 blen bidx) : blocks) =
  if idx == bidx
    then E2 blen : blocks
    else B2 blen bidx : cleanBlock idx blocks
cleanBlock idx (block : blocks) = block : cleanBlock idx blocks

--------------------------------------------------------------------------------

solvePart1 :: Problem -> Answer
solvePart1 (Input input) = solvePart1 $ parsePart1 input
solvePart1 (Problem2 _) = error "Part 1 uses Problem2 format"
solvePart1 (Problem blocks) =
  blocks |> reorder |> checkSum |> AnswerInt

checkSum :: [Block] -> Int
checkSum blocks =
  blocks |> zipWith check_number [0 ..] |> sum
  where
    check_number :: Int -> Block -> Int
    check_number _ E = 0
    check_number i (B n) = i * n

checkSum2 :: [Block2] -> Int
checkSum2 = checkSum . toBlock1
  where
    toBlock1 :: [Block2] -> [Block]
    toBlock1 [] = []
    toBlock1 ((E2 len) : blocks) = replicate len E ++ toBlock1 blocks
    toBlock1 ((B2 len idx) : blocks) = replicate len (B idx) ++ toBlock1 blocks

reorder :: [Block] -> [Block]
reorder blocks =
  sort_iterate blocks (reverse blocks) 1 len
  where
    len = length blocks
    sort_iterate :: [Block] -> [Block] -> Int -> Int -> [Block]
    sort_iterate (b : bs) (rb : rbs) i ri =
      if i == ri
        then b : replicate (len - ri) E
        else case (b, rb) of
          (_, E) -> sort_iterate (b : bs) rbs i (ri - 1)
          (B n, _) -> B n : sort_iterate bs (rb : rbs) (i + 1) ri
          (E, B n) -> B n : sort_iterate bs rbs (i + 1) (ri - 1)
    sort_iterate [] _ _ _ = []
    sort_iterate _ _ _ _ = error "unreacheable"

parsePart2 :: String -> Problem
parsePart2 input =
  input
    |> map digitToInt
    |> zipWith (\idx l -> if even idx then B2 l (idx `div` 2) else E2 l) [0 ..]
    |> Problem2

parsePart1 :: String -> Problem
parsePart1 input =
  let block_lens = map digitToInt input
      blocks = concat $ zipWith (\idx len -> replicate len $ block idx) [0 ..] block_lens
   in Problem blocks
  where
    block :: Int -> Block
    block idx =
      let (b, m) = idx `divMod` 2
       in -- all even positions are empty space
          if m == 0 then B b else E
