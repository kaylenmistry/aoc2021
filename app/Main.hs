module Main where

import Lib
import System.IO ()
import Data.List.Split ( chunksOf, splitOn, splitWhen, splitEvery )
import Data.List(sort, group, dropWhileEnd)
import Data.Char (isSpace)

main :: IO ()
main = do 
    -- day1Main
    -- day2Main
    -- day3Main
    -- day4Main
    -- day5Main
    -- day6Main
    -- day7Main
    -- day8Main
    -- day9Main
    -- day10Main
    -- day11Main
    -- day13Main
    day14Main

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
    print $ uncurry (*) posOriginal
    print "Day 2 - Part 2:"
    let posWithAim = calculatePositionWithAim $ map readCommand fileLines
    print posWithAim
    print $ (\(x, d, a) -> x * d) posWithAim

day3Main :: IO ()
day3Main = do 
    contents <- readFile "data/day3.txt"  
    let input = words contents
    print "Day 3 - Part 1:"
    print $ calculateGammaRate (map readDigits input)
    print $ calculateEpsilonRate (map readDigits input)
    print "Day 3 - Part 2:"
    print $ oxygenGeneratorRating (map readDigits input) 
    print $ co2ScrubberRating (map readDigits input)

day4Main :: IO ()
day4Main = do 
    contents <- readFile "data/day4.txt"
    let (ns:_:bs) = lines contents
    let bingoNumbers = parseListOfNumbers ns
    let boards = (map . map .map) readInt $ (map . map) (chunksOf 3) $ splitWhen (== "") bs
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
    let startFish = [0] ++ map length (group $ sort input) ++ [0, 0, 0]
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

day8Main :: IO ()
day8Main = do 
    contents <- readFile "data/day8.txt"
    print "Day 8 - Part 1:"
    let input = map (map (splitOn " " . trim) . splitOn "|") (lines contents)
    print $ sum $ map ((length . snd) . uniqueDigits) input
    print "Day 8 - Part 2:"
    print $ sum $ map getDigitOutput input

day9Main :: IO ()
day9Main = do 
    contents <- readFile "data/day9.txt" 
    print "Day 9 - Part 1:"
    let input = lines contents
    let emptyCol = replicate (2 + length (head input)) 9
    let sanitisedInput = emptyCol : map (\l -> 9 : readDigits l ++ [9]) input ++ [emptyCol]
    print $ sumLowPoints sanitisedInput
    print "Day 9 - Part 1:"

day10Main :: IO ()
day10Main = do 
    contents <- readFile "data/day10.txt"
    let input = lines contents
    print "Day 10 - Part 1:"
    print $ totalSyntaxErrorScore (map findCorruptCharacter input)
    print "Day 10 - Part 2:"
    let autocompleteScores = sort $ map (autocompleteScore . completeLine) (filterCorruptedLines input)
    print $ autocompleteScores !! div (length autocompleteScores) 2

day11Main :: IO ()
day11Main = do 
    contents <- readFile "data/day11.txt"
    let input = map (map readInt .chunksOf 1) $ lines contents
    print "Day 11 - Part 1:"
    print $ flashingOctopi input

day13Main :: IO ()
day13Main = do 
    contents <- readFile "data/day13.txt"
    let [coordsInput, foldsInput] = splitWhen (== "") (lines contents)
    let coords = map (toTuple . parseListOfNumbers) coordsInput
    let folds = map readFold foldsInput
    print "Day 13 - Part 1:"
    print $ length $ foldPaper coords [head folds]
    print "Day 13 - Part 2:"
    print "Plot the following coordinates to find the code :)"
    print $ foldPaper coords folds

day14Main :: IO ()
day14Main = do 
    contents <- readFile "data/day14.txt"
    let [[template], rulesInput] = splitWhen (== "") (lines contents)
    let rules = map (toTuple . splitOn " -> ") rulesInput
    print "Day 14 - Part 1:"
    let polymer = pairInsertion 10 template rules
    print $ polymerValue polymer (last template)
    print "Day 14 - Part 2:"
    let polymer = pairInsertion 40 template rules
    print $ polymerValue polymer (last template)

-- Helpers

readInt :: String -> Int
readInt = read

readDigits :: String -> [Int]
readDigits = map (read . pure :: Char -> Int)

readCommand :: String -> Command
readCommand ('f':'o':'r':'w':'a':'r':'d':' ': n) = Forward (readInt n)
readCommand ('u':'p':' ': n) = Up (readInt n)
readCommand ('d':'o':'w':'n':' ': n) = Down (readInt n)
readCommand s = error "invalid command"

readFold :: String -> PaperFold
readFold f = if axis == 'x' then X n else Y n
    where 
        n = readInt val
        axis = last instruction
        [instruction, val] = splitOn "=" f

toTuple :: [a] -> (a, a)
toTuple [x, y] = (x, y)

-- Parsing 

parseListOfNumbers :: String -> [Int]
parseListOfNumbers ns = map readInt $ splitOn "," (trim ns)

parseSpacedNumbers :: String -> [Int]
parseSpacedNumbers ns = map readInt $ splitOn " " (trim ns)

parseCoordinates :: [String] -> [((Int, Int), (Int, Int))]
parseCoordinates [] = []
parseCoordinates (l:ls) = ((x1,y1), (x2,y2)) : parseCoordinates ls
    where 
        [x1, y1] = map readInt $ splitOn "," to
        [x2, y2] = map readInt $ splitOn "," from
        [to, from] = splitOn "->" l

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
