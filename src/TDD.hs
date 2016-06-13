module TDD
    ( bowl, frames ) where

import Data.List

bowl :: [Int] -> Int
bowl l = sum $ map (score.reverse) (inits.frames $ l)
    where
        score [] = 0
        score (frame:prevFrame:_)
            | isSpare prevFrame = scoreFrameAfterSpare frame
            | otherwise = scoreFrame frame
        score (frame:_) = scoreFrame frame

        isSpare (roll1, roll2) = roll1 + roll2 == 10
        scoreFrame (r1, r2) = r1 + r2
        scoreFrameAfterSpare (r1, r2) = 2*r1 + r2


frames :: [Int] -> [(Int, Int)]
frames [] = []
frames (x:y:xs) = (x, y):(frames xs)
