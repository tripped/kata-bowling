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
                | frame >= 10 = 1
                | standing > 0 = max 1 $ mult - 1
                | second = 2
                | otherwise = 3
            standing = pins - roll
            rerack = standing == 0 || second
