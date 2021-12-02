module Lib ( countDepthIncreases, generateSlidingWindow, calculatePosition, calculatePositionResult, calculatePositionWithAim, Command(Forward, Up, Down)) where

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