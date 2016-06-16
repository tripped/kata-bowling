module TDD
    ( bowl ) where

data Frame = Strike | Spare Int | Open Int Int

bowl = sum . map score . take 10 . windows . frames
    where 
        -- Split a (valid) list of rolls into frames
        frames (10:xs) = Strike : frames xs
        frames [x] = [Open x 0]
        frames (x:y:xs)
            | x + y == 10 = Spare x : frames xs
            | otherwise = Open x y : frames xs

        -- Scoring is done on windows consisting of up to two following frames
        windows all@(x:xs) = take 3 all : windows xs

        score (Strike : rs) = 10 + twoRolls rs
        score (Spare _ : rs) = 10 + oneRoll rs
        score (Open x y : _) = x + y

        -- Reconstruct scorable rolls from a sequence of frames
        rolls (Open x y : fs) = x : y : rolls fs
        rolls (Spare x : fs) = x : 10 - x : rolls fs
        rolls (Strike : fs) = 10 : rolls fs

        twoRolls = sum . take 2 . rolls
        oneRoll = sum . take 1 . rolls
