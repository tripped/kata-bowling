module NonTDD
  ( bowlingScore
  , playGame
  ) where

import Data.List

-- * Non-TDD Flavor * --
-- This version of the bowling game score function was written entirely from
-- ad-hoc design; no tests were written until the initial implementation was
-- done. Gaze upon its craptacularity!

-- The initial state of a game of bowling.
-- Score:           0
-- Pins standing:   10
-- Frame:           1
-- Second roll?     False
-- Multipliers:     (1, 1) == (current roll, next roll)
initialState = (0, (10, 1, False, (1, 1)))

-- Type alias, just for signature brevity
type GameState = (Int, (Int, Int, Bool, (Int, Int)))

playGame :: [Int] -> GameState
playGame = foldl' scoreOne initialState

bowlingScore :: [Int] -> Int
bowlingScore = fst . playGame

scoreOne :: GameState -> Int -> GameState
scoreOne (score, (pins, frame, second, mult)) roll =
  (score', (pins', frame', second', mult')) where
    (bonus, next) = mult
    score' = score + roll * bonus
    pins'
      | rerack = 10
      | otherwise = standing
    frame'
      | rerack = frame + 1
      | otherwise = frame
    second' = not rerack
    mult'
      | standing > 0 || frame >= 10 = (next, 1)   -- No bonus after 10th frame
      | second = (next + 1, 1)                    -- Spare!
      | otherwise = (next + 1, 2)                 -- Strike!
    standing = pins - roll
    rerack = standing == 0 || second              -- Rerack on clear/second roll
