module TDD
    ( bowl ) where

import Data.List

data Frame = Strike | Spare Int | Open Int Int

bowl = sum . map score . take 10 . tails . frames
    where 
        -- Split a (valid) list of rolls into frames
        frames (10:xs) = Strike : frames xs
        frames [x] = [Open x 0]
        frames (x:y:xs)
            | x + y == 10 = Spare x : frames xs
            | otherwise = Open x y : frames xs

        -- Reconstruct scorable rolls from a sequence of frames
        rolls (Open x y : fs) = x : y : rolls fs
        rolls (Spare x : fs) = x : 10 - x : rolls fs
        rolls (Strike : fs) = 10 : rolls fs

        -- Each frame can be scored by its rolls and up to 2 following rolls
        score (Strike : after) = 10 + sum (take 2 $ rolls after)
        score (Spare _ : after) = 10 + sum (take 1 $ rolls after)
        score (Open x y : _) = x + y
