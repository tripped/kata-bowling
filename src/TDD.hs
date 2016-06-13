module TDD
    ( bowl ) where

import Data.List

bowl :: [Int] -> Int
bowl l = sum $ map (score.reverse) (inits l)
    where
        score :: [Int] -> Int
        score [] = 0
        score [x] = x
        score [x,y] = x
        score (x:y:z:xs)
            | y + z == 10 = 2*x
            | otherwise = x
