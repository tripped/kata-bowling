module TDD
    ( bowl ) where

data Frame = Strike | Spare Int | Open Int Int

bowl = sum . map score . take 10 . windows . frames
    where 
        -- Split a (valid) list of rolls into frames
        frames (10:xs) = Strike : (frames xs)
        frames [x] = [Open x 0]
        frames (x:y:xs)
            | x + y == 10 = Spare x : (frames xs)
            | otherwise = Open x y : (frames xs)

        -- Scoring is done on windows consisting of up to two following frames
        windows all@(x:xs) = take 3 all : windows xs

        score (Strike : rs) = 10 + twoRolls rs
        score (Spare _ : rs) = 10 + oneRoll rs
        score (Open x y : _) = x + y

        twoRolls (Open x y : _) = x + y
        twoRolls (Spare _ : _) = 10
        twoRolls (Strike : rs) = 10 + oneRoll rs

        oneRoll (Open x _ : _) = x
        oneRoll (Spare x : _) = x
        oneRoll (Strike : _) = 10
