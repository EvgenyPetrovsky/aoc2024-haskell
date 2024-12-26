{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day21
  ( Problem (..),
    Answer (..),
  )
where

import Lib (Answer (..))
import qualified Lib as L (Problem (..))
import Util ( (|>), filterNot )
import qualified Data.Map.Strict as Map

data Key = K0 | K1 | K2 | K3 | K4 | K5 | K6 | K7 | K8 | K9 | KA | KU | KD | KL | KR | K_
  deriving (Ord, Eq)

instance Show Key where
  show K0 = "0"
  show K1 = "1"
  show K2 = "2"
  show K3 = "3"
  show K4 = "4"
  show K5 = "5"
  show K6 = "6"
  show K7 = "7"
  show K8 = "8"
  show K9 = "9"
  show KA = "A"
  show KU = "^"
  show KD = "v"
  show KL = "<"
  show KR = ">"
  show K_ = " "

type Location = (Int, Int)
type Layout = Map.Map Key Location

data Problem = Input String | Problem [KeySequence]

instance Show Problem where
  show (Input input) = "unparsed iput" ++ input
  show (Problem codes) = "\n" ++ map (concatMap show) codes |> unlines
type KeySequence = [Key]
type State = (Key, KeySequence)

instance L.Problem Problem where
  parse1 = parsePart1
  solve1 = solvePart1
  solve2 = solvePart2

--------------------------------------------------------------------------------

solvePart2 :: Problem -> Answer
solvePart2 (Input input) = solvePart2 $ parsePart1 input
solvePart2 (Problem _) = Unknown

--------------------------------------------------------------------------------

-- 225748 - too high
-- 217784 - too high

-- 179A gives difference
solvePart1 :: Problem -> Answer
solvePart1 (Input input) = solvePart1 $ parsePart1 input
solvePart1 (Problem codes) = codes |> map complexity |> sum |> AnswerInt

complexity :: KeySequence -> Int
complexity original_code =
  keysToInt original_code * (length . encode1 . encode1 . encode0 $ original_code)
  where
    encode0 seq = encode seq numberPad
    encode1 seq = encode seq cursorPad

keysToInt :: KeySequence -> Int
keysToInt keys =
  keys |> filterNot (== KA) |> foldl (\z k -> z * 10 + keyToInt k) 0
  where
    keyToInt k = case k of
      K0 -> 0
      K1 -> 1
      K2 -> 2
      K3 -> 3
      K4 -> 4
      K5 -> 5
      K6 -> 6
      K7 -> 7
      K8 -> 8
      K9 -> 9
      x -> error $ "unexpected key: " ++ show x


encode :: KeySequence -> Layout -> KeySequence
encode in_seq layout =
  foldl keypress (KA,[]) in_seq |> snd
  where
    keypress :: State -> Key -> State
    keypress (location,acc_seq) key =
      (key , acc_seq ++ moveToKey location key layout ++ [KA])

moveToKey :: Key -> Key -> Layout -> KeySequence
-- moveToKey KA KL _ = [KD,KL,KL]
-- moveToKey KA KD _ = [KD,KL]
moveToKey from_key to_key on_layout =
  let (r1,c1) = on_layout Map.! from_key
      (r2,c2) = on_layout Map.! to_key
      (dr,dc) = (r2-r1,c2-c1)
   in move dr dc
      --move_ver dr ++ move_hor dc
  where
    -- move_hor n = if n >= 0 then replicate n KR else replicate (abs n) KL
    -- move_ver n = if n >= 0 then replicate n KU else replicate (abs n) KD
    move dr dc
      | dr < 0 && dc < 0 = replicate (abs dr) KD ++ replicate (abs dc) KL
      | dr < 0 && dc >= 0 = replicate dc KR ++ replicate (abs dr) KD
      | dr >= 0 && dc < 0 = replicate (abs dc) KL ++ replicate dr KU
      | dr >= 0 && dc >= 0 = replicate dr KU ++ replicate dc KR
      | otherwise = error $ "strange case: dr=" ++ show dr ++ ", dc=" ++ show dc
numberPad :: Layout
numberPad = Map.fromList [
   (K7,(4,1)),(K8,(4,2)),(K9,(4,3))
  ,(K4,(3,1)),(K5,(3,2)),(K6,(3,3))
  ,(K1,(2,1)),(K2,(2,2)),(K3,(2,3))
  ,(K_,(1,1)),(K0,(1,2)),(KA,(1,3))]
cursorPad :: Layout
cursorPad = Map.fromList [
   (K_,(1,1)),(KU,(2,2)),(KA,(2,3))
  ,(KL,(1,1)),(KD,(1,2)),(KR,(1,3))]

--------------------------------------------------------------------------------

parsePart1 :: String -> Problem
parsePart1 input =
  input |> lines |> map (map key) |> Problem
  where
    key :: Char -> Key
    key '0' = K0
    key '1' = K1
    key '2' = K2
    key '3' = K3
    key '4' = K4
    key '5' = K5
    key '6' = K6
    key '7' = K7
    key '8' = K8
    key '9' = K9
    key 'A' = KA
    key k = error $ "Unknown key: " ++ [k]


--------------------------------------------------------------------------------
