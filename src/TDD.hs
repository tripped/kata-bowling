module TDD
    ( bowl ) where

import Data.List

data Frame = Strike | Spare Int | Open Int Int
    deriving Show

windows :: [Frame] -> [[Frame]]
windows [] = []
windows all@(x:xs) = take 3 all : windows xs

bowl l = sum $ map score $ take 10 (windows.frames $ l)
    where 
        score (Strike : rs) = 10 + twoRolls rs
        score (Spare _ : rs) = 10 + oneRoll rs
        score (Open x y : _) = x + y

        twoRolls [] = 0
        twoRolls (Open x y : _) = x + y
        twoRolls (Spare _ : _) = 10
        twoRolls (Strike : rs) = 10 + oneRoll rs

        oneRoll [] = 0
        oneRoll (Open x _ : _) = x
        oneRoll (Spare x : _) = x
        oneRoll (Strike : _) = 10

frames :: [Int] -> [Frame]
frames [] = []
frames (10:xs) = Strike:(frames xs)
frames (x:y:xs)
    | x + y == 10 = (Spare x):(frames xs)
    | otherwise = (Open x y):(frames xs)
