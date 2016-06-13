module TDD
    ( bowl, frames ) where

import Data.List

bowl :: [Int] -> Int
bowl l = sum $ map (score.reverse) (inits.frames $ l)
    where
        score [] = 0
        score (frame:prevFrame:_)
            | isStrike prevFrame = scoreFrameAfterStrike frame
            | isSpare prevFrame = scoreFrameAfterSpare frame
            | otherwise = scoreFrame frame
        score (frame:_) = scoreFrame frame

        isSpare (r1, r2) = r1 + r2 == 10
        isStrike frame = frame == (10, 0)
        scoreFrame (r1, r2) = r1 + r2
        scoreFrameAfterSpare (r1, r2) = 2*r1 + r2
        scoreFrameAfterStrike (r1, r2) = 2*(r1 + r2)

frames :: [Int] -> [(Int, Int)]
frames [] = []
frames (10:xs) = (10, 0):(frames xs)
frames (x:y:xs) = (x, y):(frames xs)
