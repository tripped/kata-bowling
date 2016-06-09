module Lib
    ( bowlingScore
    ) where

--bowlingScore :: [Int] -> (Int, (Int, Int, Bool, Int))
bowlingScore :: [Int] -> Int
bowlingScore = fst . foldl m (0, (10, 1, False, 1)) where
    m :: (Int, (Int, Int, Bool, Int)) -> Int -> (Int, (Int, Int, Bool, Int))
    m (score, (pins, frame, second, mult)) roll =
        (score', (pins', frame', second', mult')) where
            score' = score + roll * mult
            pins'
                | standing == 0 || second = 10
                | otherwise = standing
            frame'
                | standing == 0 || second = frame + 1
                | otherwise = frame
            second' = not $ second || standing == 0
            mult'
                | frame >= 10 = 1
                | standing > 0 = max 1 $ mult - 1
                | second = 2
                | otherwise = 3
            standing = pins - roll
