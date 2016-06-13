module TDD
    ( bowl, frames ) where

import Data.List

data Frame = Strike | Spare Int | Open Int Int
    deriving Show

bowl :: [Int] -> Int
bowl l = sum $ map (score.reverse) (inits.frames $ l)
    where
        score (f:Strike:Strike:_) = scoreAfterDoubleStrike f
        score (f:Strike:_) = scoreAfterStrike f
        score (f:Spare _:_) = scoreAfterSpare f
        score (f:_) = scoreNormal f
        score _ = 0

        scoreAfterDoubleStrike Strike = 30
        scoreAfterDoubleStrike (Spare x) = 20 + x
        scoreAfterDoubleStrike (Open x y) = 3*x + 2*y

        scoreAfterStrike Strike = 20
        scoreAfterStrike (Spare _) = 20
        scoreAfterStrike (Open x y) = 2*(x + y)

        scoreAfterSpare Strike = 20
        scoreAfterSpare (Spare x) = 10 + x
        scoreAfterSpare (Open x y) = 2*x + y

        scoreNormal Strike = 10
        scoreNormal (Spare _) = 10
        scoreNormal (Open x y) = x + y

frames :: [Int] -> [Frame]
frames [] = []
frames (10:xs) = Strike:(frames xs)
frames (x:y:xs)
    | x + y == 10 = (Spare x):(frames xs)
    | otherwise = (Open x y):(frames xs)
