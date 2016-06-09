module Main where

import Lib

main :: IO ()
main = putStrLn $ show $ bowlingScore $ take 12 $ repeat 10
