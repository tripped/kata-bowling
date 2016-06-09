module Lib
    ( bowlingScore
    ) where

bowlingScore :: [Int] -> Int
bowlingScore = fst . foldl m (0, (10, 1, False, 1)) where
  m (score, (pins, frame, second, mult)) roll =
    (score', (pins', frame', second', mult')) where
      score' = score + roll * mult
      pins'
        | rerack = 10
        | otherwise = standing
      frame'
        | rerack = frame + 1
        | otherwise = frame
      second' = not rerack
      mult'
        | frame >= 10 = 1                   -- No mult after frame 10
        | standing > 0 = max 1 $ mult - 1   -- Crap!
        | second = 2                        -- Spare!
        | otherwise = 3                     -- Strike!
      standing = pins - roll
      rerack = standing == 0 || second      -- Rerack on clear or second roll
