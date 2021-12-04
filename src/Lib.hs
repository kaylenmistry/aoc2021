module Lib ( 
    countDepthIncreases, generateSlidingWindow, 
    calculatePosition, calculatePositionWithAim, Command(Forward, Up, Down),
    calculateGammaRate, calculateEpsilonRate, oxygenGeneratorRating, co2ScrubberRating,
    playBingo, createBingoBoards, bingoScore
) where

import Data.List(transpose)

-- Day 1: AOC 2021

countDepthIncreases :: Ord a => [a] -> Int
countDepthIncreases xs = countDepthIncreases' xs 0

countDepthIncreases' :: Ord a => [a] -> Int -> Int
countDepthIncreases' [] n = n
countDepthIncreases' [x] n = n
countDepthIncreases' (x1:x2:xs) n = countDepthIncreases' (x2:xs) count
    where count = if x2 > x1 then (n + 1) else n

generateSlidingWindow :: Num a => [a] -> [a]
generateSlidingWindow (x1:x2:x3:xs) = (x1 + x2 + x3) : (generateSlidingWindow (x2:x3:xs))
generateSlidingWindow xs = []

-- Day 2: AOC 2021

data Command = Forward Int | Up Int | Down Int deriving (Show)

calculatePositionResult :: (Int, Int) -> Int
calculatePositionResult (x, y) = (x * y)

calculatePosition :: [Command] -> (Int, Int)
calculatePosition cs = foldl calculateNewPosition (0, 0) cs

calculateNewPosition :: (Int, Int) -> Command -> (Int, Int)
calculateNewPosition (x, d) (Forward n) = (x + n, d)
calculateNewPosition (x, d) (Up n) = (x, d - n)
calculateNewPosition (x, d) (Down n) = (x, d + n)

calculatePositionWithAim :: [Command] -> (Int, Int, Int)
calculatePositionWithAim cs = foldl calculateNewPositionWithAim (0, 0, 0) cs

calculateNewPositionWithAim :: (Int, Int, Int) -> Command -> (Int, Int, Int)
calculateNewPositionWithAim (x, d, aim) (Forward n) = (x + n, d + (aim * n), aim)
calculateNewPositionWithAim (x, d, aim) (Up n) = (x, d, aim - n)
calculateNewPositionWithAim (x, d, aim) (Down n) = (x, d, aim + n)

-- Day 3: AOC 2021

calculateGammaRate :: [[Int]] -> [Int]
calculateGammaRate xs = map (\b -> if b < 0 then 0 else 1) (countBitOccurrences xs)

calculateEpsilonRate :: [[Int]] -> [Int]
calculateEpsilonRate xs = map (\b -> if b < 0 then 1 else 0) (countBitOccurrences xs)

countBitOccurrences :: [[Int]] -> [Int]
countBitOccurrences xs = foldl (zipWith (\c b -> if b == 1 then c + 1 else c - 1)) (repeat 0) xs


oxygenGeneratorRating :: [[Int]] -> [Int]
oxygenGeneratorRating [x] = x
oxygenGeneratorRating ([]:xs) = []
oxygenGeneratorRating xs = bit : (oxygenGeneratorRating $ map tail $ filter (\bs -> bit == head bs) xs)
    where 
        bit = if bitCount < 0 then 0 else 1
        bitCount = foldl (\c b -> if b == 1 then c + 1 else c - 1) 0 (map head xs)

co2ScrubberRating :: [[Int]] -> [Int]
co2ScrubberRating [x] = x
co2ScrubberRating ([]:xs) = []
co2ScrubberRating xs = bit : (co2ScrubberRating $ map tail $ filter (\bs -> bit == head bs) xs)
    where 
        bit = if bitCount < 0 then 1 else 0
        bitCount = foldl (\c b -> if b == 1 then c + 1 else c - 1) 0 (map head xs)

-- Day 4: AOC 2021

bingoScore :: Int -> [[Maybe Int]] -> Maybe Int 
bingoScore n board = fmap sum $ sequence $ filter (\x -> x /= Nothing) (concat board)

playBingo :: [Int] -> [[[Maybe Int]]] -> (Int, [[Maybe Int]])
playBingo (n:ns) boards = if length bingoBoards > 0 then (n, head bingoBoards) else playBingo ns updatedBoards
    where
        bingoBoards = filter (checkForBingo) updatedBoards
        updatedBoards = map (maybeMarkBoard n) boards

checkForBingo :: [[Maybe Int]] -> Bool
checkForBingo board = checkForBingoRows board || checkForBingoRows (transpose board)

checkForBingoRows :: [[Maybe Int]] -> Bool
checkForBingoRows board = foldl (||) False $ map (foldl (\a x -> a && x == Nothing) True) board

createBingoBoards :: [[[Int]]] -> [[[Maybe Int]]]
createBingoBoards = map . map . map $ (\x -> Just x)

maybeMarkBoard :: Int -> [[Maybe Int]] -> [[Maybe Int]]
maybeMarkBoard x board = (map . map) (markBoard x) board 

markBoard :: Int -> Maybe Int -> Maybe Int
markBoard _ Nothing = Nothing
markBoard x (Just y) = if x == y then Nothing else Just y