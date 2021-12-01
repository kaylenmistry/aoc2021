module Main where

import Lib
import System.IO

main :: IO ()
main = do 
    contents <- readFile "data/day1-depths.txt"  
    print . countDepthIncreases . generateSlidingWindow . map readInt . words $ contents

readInt :: String -> Int
readInt = read