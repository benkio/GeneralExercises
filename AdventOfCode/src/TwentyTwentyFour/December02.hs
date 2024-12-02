module TwentyTwentyFour.December02 where

import Data.List (subsequences)
import Data.Maybe (mapMaybe)

type Reports = [Levels]
type Levels = [Int]

input :: IO Reports
input = parseInput <$> readFile "input/2024/December02.txt"

parseInput :: String -> Reports
parseInput = fmap parseReport . lines
  where
    parseReport :: String -> Levels
    parseReport = fmap (\x -> read x :: Int) . words

testInput :: Reports
testInput =
    parseInput
        "7 6 4 2 1\n\
        \1 2 7 8 9\n\
        \9 7 6 2 1\n\
        \1 3 2 4 5\n\
        \8 6 4 4 1\n\
        \1 3 6 7 9"

levelInOrder :: Levels -> Bool
levelInOrder ls = all (uncurry (<)) levelsPair || all (uncurry (>)) levelsPair
  where
    levelsPair = zip ls (tail ls)
levelDiffer :: Levels -> Bool
levelDiffer ls = all (\(v1, v2) -> abs (v1 - v2) `elem` [1 .. 3]) levelsPair
  where
    levelsPair = zip ls (tail ls)
levelCheck :: Levels -> Bool
levelCheck l = levelInOrder l && levelDiffer l

solution :: (Levels -> Bool) -> Reports -> Int
solution checkFn = length . filter checkFn

december02Solution1 :: IO Int
december02Solution1 = solution levelCheck <$> input

levelCheck2 :: Levels -> Bool
levelCheck2 ls = (any levelCheck . filter ((>= (length ls - 1)) . length) . subsequences) ls

december02Solution2 :: IO Int
december02Solution2 = solution levelCheck2 <$> input
