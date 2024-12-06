module Day06
    ( Problem (..)
    , Answer (..)
    ) where

import qualified Lib as L ( Problem (..) )
import Lib ( Answer (..) )
--import Util

-- type Answer = L.Answer
data Definition = Definition
data Problem = Input String | Problem Definition

instance L.Problem Problem where
    parse1 _ = Problem Definition
    solve1 _ = Unknown
    solve2 _ = Unknown

--------------------------------------------------------------------------------