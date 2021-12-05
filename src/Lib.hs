module Lib ( 
    countDepthIncreases, generateSlidingWindow, 
    calculatePosition, calculatePositionWithAim, Command(Forward, Up, Down),
    calculateGammaRate, calculateEpsilonRate, oxygenGeneratorRating, co2ScrubberRating,
    playBingo, createBingoBoards, bingoScore, findBingoWinner, findLastBingoWinner,
    generateCoordinateSpace, getCollisions
) where

import Data.List(transpose, partition)
import Data.Map (Map)
import qualified Data.Map as Map

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

bingoScore :: (Int, [[Maybe Int]]) -> Maybe Int 
bingoScore (n, board) = fmap sum $ sequence $ filter (\x -> x /= Nothing) (concat board)

findBingoWinner :: [(Int, [[[Maybe Int]]])] -> (Int, [[Maybe Int]]) 
findBingoWinner ((n, []): xs) = findBingoWinner xs
findBingoWinner ((n, bs): xs) = (n, head bs)

findLastBingoWinner :: [(Int, [[[Maybe Int]]])] -> (Int, [[Maybe Int]])
findLastBingoWinner = findBingoWinner . reverse

playBingo :: [Int] -> [[[Maybe Int]]] -> [(Int, [[[Maybe Int]]])]
playBingo [] _ = []
playBingo _ [] = []
playBingo (n:ns) boards = (n, bingoBoards) : (playBingo ns updatedBoards)
    where
        (bingoBoards, updatedBoards) = partition (checkForBingo) $ map (maybeMarkBoard n) boards

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

-- Day 5: AOC 2021

getCollisions :: Map (Int, Int) Int -> [(Int, Int)]
getCollisions = Map.keys . Map.filter (> 1)

generateCoordinateSpace :: [((Int, Int), (Int, Int))] -> Map (Int, Int) Int
generateCoordinateSpace lines = generateCoordinateSpace' lines Map.empty

generateCoordinateSpace' :: [((Int, Int), (Int, Int))] -> Map (Int, Int) Int -> Map (Int, Int) Int
generateCoordinateSpace' [] m = m
generateCoordinateSpace' ((to, from):xs) m = generateCoordinateSpace' xs updatedMap
    where 
        updatedMap = updateMap coordinates m
        coordinates = lineCoordinates to from

updateMap :: [(Int, Int)] -> Map (Int, Int) Int -> Map (Int, Int) Int
updateMap [] m = m
updateMap (x:xs) m = updateMap xs (Map.insertWith (+) x 1 m)

lineCoordinates :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
lineCoordinates (x1, y1) (x2, y2) 
    | x1 == x2 && y1 == y2 = [(x1, y1)]
    | x1 == x2  = [(x1, y) | y <- [min y1 y2 .. max y1 y2]]
    | y1 == y2  = [(x, y1) | x <- [min x1 x2 .. max x1 x2]]
    | otherwise = (x1, y1) : lineCoordinates (x, y) (x2, y2)
        where x = if x1 < x2 then x1 + 1 else x1 - 1
              y = if y1 < y2 then y1 + 1 else y1 - 1
