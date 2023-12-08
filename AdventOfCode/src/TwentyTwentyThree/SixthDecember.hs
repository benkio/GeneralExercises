module TwentyTwentyThree.SixthDecember where

import Data.List (transpose)

data RaceRecord = RR {time :: Int, distance :: Int} deriving (Show)

input :: IO [RaceRecord]
input = parseInput <$> readFile "input/2023/6December.txt"

parseInput :: String -> [RaceRecord]
parseInput = fmap (\[t, d] -> RR{time = read t :: Int, distance = read d :: Int}) . transpose . fmap (tail . words) . lines

testInput :: [RaceRecord]
testInput =
    parseInput
        "Time:      7  15   30\n\
        \Distance:  9  40  200"

raceDistances :: Int -> [Int]
raceDistances t = calcDistance <$> [0 .. t]
  where
    calcDistance ct = (t - ct) * ct

raceBreakingRecord :: RaceRecord -> Int
raceBreakingRecord (RR{time = t, distance = d}) =
    (length . filter (> d) . raceDistances) t

solution1 :: [RaceRecord] -> Int
solution1 = product . fmap raceBreakingRecord

input' :: IO RaceRecord
input' = parseInput' <$> readFile "input/2023/6December.txt"

parseInput' :: String -> RaceRecord
parseInput' = (\[t, d] -> RR{time = read t :: Int, distance = read d :: Int}) . fmap (concat . tail . words) . lines

testInput' :: RaceRecord
testInput' =
    parseInput'
        "Time:      7  15   30\n\
        \Distance:  9  40  200"

sixthDecemberSolution1 :: IO Int
sixthDecemberSolution1 = solution1 <$> input

solution2 = raceBreakingRecord

sixthDecemberSolution2 :: IO Int
sixthDecemberSolution2 = raceBreakingRecord <$> input'
