module TwentyTwentyFour.December01 where

import Data.Bifunctor (bimap, second)
import Data.List (group, sort)
import Data.Map (Map, fromList, (!?))
import Data.Maybe (fromMaybe)

input :: IO [(Int, Int)]
input = parseInput <$> readFile "input/2024/December01.txt"

parseInput :: String -> [(Int, Int)]
parseInput = fmap parseLine . lines
  where
    parseLine :: String -> (Int, Int)
    parseLine s = (parseNum s, (parseNum . dropWhile (== ' ') . dropWhile (/= ' ')) s)
    parseNum = (\x -> read x :: Int) . takeWhile (/= ' ')

testInput :: [(Int, Int)]
testInput =
    parseInput
        "3   4\n\
        \4   3\n\
        \2   5\n\
        \1   3\n\
        \3   9\n\
        \3   3"

solution1 :: [(Int, Int)] -> Int
solution1 =
    foldl
        (\acc (v1, v2) -> acc + abs (v1 - v2))
        0
        . reorg
  where
    reorg :: [(Int, Int)] -> [(Int, Int)]
    reorg = uncurry zip . bimap sort sort . unzip

december01Solution1 :: IO Int
december01Solution1 = solution1 <$> input

solution2 :: [(Int, Int)] -> Int
solution2 = (\(ks, vm) -> foldl (\acc k -> acc + (k * fromMaybe 0 (vm !? k))) 0 ks) . reorg
  where
    reorg :: [(Int, Int)] -> ([Int], Map Int Int)
    reorg = second (fromList . map (\ls -> (head ls, length ls)) . group . sort) . unzip

december01Solution2 :: IO Int
december01Solution2 = solution2 <$> input
