{-# LANGUAGE OverloadedStrings #-}

module TwentyTwentyOne.SeventhDecember where

import Data.Text (Text)
import qualified Data.Text as Text (pack, splitOn, unpack)

input :: IO [Int]
input = parseInput <$> readFile "input/2021/7December.txt"

parseInput :: String -> [Int]
parseInput = fmap ((\x -> read x :: Int) . Text.unpack) . Text.splitOn "," . Text.pack

inputTest :: [Int]
inputTest = parseInput "16,1,2,0,4,2,7,1,2,14"

solution :: (Int -> Int -> [Int]) -> [Int] -> Int
solution stepCost xs =
    let max = maximum xs
        accumulator = replicate (max + 1) 0
        deltas = fmap (stepCost max) xs
        sumDeltas = foldr (zipWith (+)) accumulator deltas
     in minimum sumDeltas

solution1StepCost :: Int -> Int -> [Int]
solution1StepCost max x = fmap (\y -> abs (y - x)) [0 .. max]

solution2StepCost :: Int -> Int -> [Int]
solution2StepCost max x = fmap (\y -> let d = abs (y - x) in (d * (d + 1)) `div` 2) [0 .. max]

seventhDecemberSolution1 :: IO Int
seventhDecemberSolution1 = solution solution1StepCost <$> input

seventhDecemberSolution2 :: IO Int
seventhDecemberSolution2 = solution solution2StepCost <$> input
