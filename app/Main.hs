module Main where

import NonTDD

main :: IO ()
main = putStrLn $ show $ bowlingScore $ take 12 $ repeat 10
