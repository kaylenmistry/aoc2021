module Main where

import Lib
import System.IO
import Data.List.Split
import Data.List(sort, group)

main :: IO ()
main = do 
    -- day1Main
    -- day2Main
    -- day3Main
    -- day4Main
    -- day5Main
    -- day6Main
    day7Main

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

day3Main :: IO ()
day3Main = do 
    contents <- readFile "data/day3.txt"  
    let input = words contents
    print "Day 3 - Part 1:"
    print $ calculateGammaRate (map readBinary input)
    print $ calculateEpsilonRate (map readBinary input)
    print "Day 3 - Part 2:"
    print $ oxygenGeneratorRating (map readBinary input) 
    print $ co2ScrubberRating (map readBinary input)

day4Main :: IO ()
day4Main = do 
    contents <- readFile "data/day4.txt"
    let (ns:_:bs) = lines contents
    let bingoNumbers = parseListOfNumbers ns
    let boards = (map . map .map) readInt $ (map . map) (chunksOf 3) $ splitWhen (\x -> x == "") bs
    print "Day 4 - Part 1:"
    let results = playBingo bingoNumbers (createBingoBoards boards)
    print $ bingoScore $ findBingoWinner results
    print "Day 4 - Part 2:"
    print $ bingoScore $ findLastBingoWinner results

day5Main :: IO ()
day5Main = do
    contents <- readFile "data/day5.txt"
    let input = parseCoordinates $ lines contents
    print "Day 5 - Part 1:"
    -- TODO: comment out the diagonal line coordinate generator
    -- print $ length $ getCollisions $ generateCoordinateSpace input
    print "Day 5 - Part 2:"
    print $ length $ getCollisions $ generateCoordinateSpace input

day6Main :: IO ()
day6Main = do
    contents <- readFile "data/day6.txt" 
    let input = parseListOfNumbers $ head $ lines contents
    let startFish = [0] ++ (map length $ group $ sort input) ++ [0, 0, 0]
    print "Day 6 - Part 1:"
    print $ sum $ lanternfish 80 startFish
    print "Day 6 - Part 2:"
    print $ sum $ lanternfish 256 startFish

day7Main :: IO ()
day7Main = do 
    contents <- readFile "data/day7.txt" 
    print "Day 7 - Part 1:"
    let input = parseListOfNumbers $ head $ lines contents
    print $ alignCrabs input 
    print "Day 7 - Part 2:"
    print $ alignExponentialCrabs input


-- Helpers

readInt :: String -> Int
readInt = read

readBinary :: String -> [Int]
readBinary = map (read . pure :: Char -> Int)

readCommand :: String -> Command
readCommand ('f':'o':'r':'w':'a':'r':'d':' ': n) = Forward (readInt n)
readCommand ('u':'p':' ': n) = Up (readInt n)
readCommand ('d':'o':'w':'n':' ': n) = Down (readInt n)

-- Parsing 

parseListOfNumbers :: String -> [Int]
parseListOfNumbers ns = map readInt $ splitOn "," ns

parseCoordinates :: [String] -> [((Int, Int), (Int, Int))]
parseCoordinates [] = []
parseCoordinates (l:ls) = ((x1,y1), (x2,y2)) : (parseCoordinates ls)
    where 
        [x1, y1] = map readInt $ splitOn "," to
        [x2, y2] = map readInt $ splitOn "," from
        [to, from] = splitOn "->" l