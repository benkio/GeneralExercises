module TwentyTwentyFive.December01 where

import Lib.Parse

startingpoint :: Int
startingpoint = 50

input :: IO [(String, Int)]
input = parseLeftRightNumber <$> readFile "input/2025/December01.txt"

parseInput :: String -> [(String, Int)]
parseInput = parseLeftRightNumber

testInput :: [(String, Int)]
testInput =
    parseInput
        "L68\n\
        \L30\n\
        \R48\n\
        \L5\n\
        \R60\n\
        \L55\n\
        \L1\n\
        \L99\n\
        \R14\n\
        \L82\n"

rotateDial :: Int -> (String, Int) -> Int
rotateDial dial ("R", value) = (dial + value) `mod` 100
rotateDial dial ("L", value) = if out < 0 then 100 + out else out
  where
    out = (dial - value) `mod` 100

solution :: ((Int, Int) -> (String, Int) -> (Int, Int)) -> [(String, Int)] -> Int
solution go = snd . foldl go (startingpoint, 0)

rotateNCount :: (Int, Int) -> (String, Int) -> (Int, Int)
rotateNCount (dial, count) rotation = if newDial == 0 then (newDial, count + 1) else (newDial, count)
  where
    newDial = rotateDial dial rotation

december01Solution1 :: IO Int
december01Solution1 = solution rotateNCount <$> input

rotateNCount' :: (Int, Int) -> (String, Int) -> (Int, Int)
rotateNCount' (dial, count) rotation@(r, clicks)
    | dial == 0 = (newDial, count')
    | (newDial == 0) || (restClicks + dial > 100 && restClicks + dial < 200 && r == "R")
        || (dial - restClicks < 0 && dial - restClicks > -100 && r == "L") =
        (newDial, count' + 1)
    | otherwise =
        (newDial, count')
  where
    newDial = rotateDial dial rotation
    restClicks = clicks `mod` 100
    fullRounds = abs (clicks `div` 100)
    count' = count + fullRounds

-- 5640 too low
december01Solution2 :: IO Int
december01Solution2 = solution rotateNCount' <$> input
