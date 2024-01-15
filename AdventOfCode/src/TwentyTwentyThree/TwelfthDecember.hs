module TwentyTwentyThree.TwelfthDecember where

import Data.List.Split (splitOn)

data SpringRows = SR {damagedRecord :: String, damagedGroups :: [Int]} deriving (Show)

solution = undefined

twelfthDecemberSolution1 :: IO Int
twelfthDecemberSolution1 = undefined

solution2 = undefined

twelfthDecemberSolution2 :: IO Int
twelfthDecemberSolution2 = undefined

input :: IO [SpringRows]
input = parseInput <$> readFile "input/2023/12December.txt"

parseInput :: String -> [SpringRows]
parseInput = fmap parseSpringRow . lines
  where
    parseSpringRow = (\[dr, dg] -> SR{damagedRecord = dr, damagedGroups = (fmap (\x -> read x :: Int) . splitOn ",") dg}) . words

testInput :: [SpringRows]
testInput =
    parseInput
        "???.### 1,1,3\n\
        \.??..??...?##. 1,1,3\n\
        \?#?#?#?#?#?#?#? 1,3,1,6\n\
        \????.#...#... 4,1,1\n\
        \????.######..#####. 1,6,5\n\
        \?###???????? 3,2,1"
