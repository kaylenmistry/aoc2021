module Main where

import Lib
import System.IO

main :: IO ()
main = do 
    day1Main
    day2Main

-- Outputs

day1Main :: IO ()
day1Main = do 
    contents <- readFile "data/day1.txt"  
    print "Day 1 - Part 1:"
    print . countDepthIncreases . map readInt . words $ contents
    print "Day 1 - Part 2:"
    print . countDepthIncreases . generateSlidingWindow . map readInt . words $ contents

day2Main :: IO ()
day2Main = do 
    contents <- readFile "data/day2.txt"  
    let fileLines = lines contents
    print "Day 2 - Part 1:"
    let posOriginal = calculatePosition $ map readCommand fileLines
    print posOriginal
    print $ (\(x, d) -> x * d) posOriginal
    print "Day 2 - Part 2:"
    let posWithAim = calculatePositionWithAim $ map readCommand fileLines
    print posWithAim
    print $ (\(x, d, a) -> x * d) posWithAim

-- Helpers

readInt :: String -> Int
readInt = read

readCommand :: String -> Command
readCommand ('f':'o':'r':'w':'a':'r':'d':' ': n) = Forward (readInt n)
readCommand ('u':'p':' ': n) = Up (readInt n)
readCommand ('d':'o':'w':'n':' ': n) = Down (readInt n)