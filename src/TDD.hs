module TDD
    ( bowl, frames ) where

import Data.List

bowl :: [Int] -> Int
bowl l = sum $ map (score.reverse) (inits.frames $ l)
    where
        score :: [(Int, Int)] -> Int
        score [] = 0
        score ((r1,r2):(prev1,prev2):_)
            | prev1 + prev2 == 10 = 2*r1 + r2
            | otherwise = r1 + r2
        score ((r1,r2):_) = r1 + r2

frames :: [Int] -> [(Int, Int)]
frames [] = []
frames (x:y:xs) = (x, y):(frames xs)
