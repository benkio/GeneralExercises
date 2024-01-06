module TwentyTwentyThree.TwelfthDecember where

import Data.List.Split

data SpringRows = SR {damagedRecord :: String, damagedGroups :: [Int]} deriving (Show)

input :: IO [SpringRows]
input = parseInput <$> readFile "input/2023/12December.txt"

parseInput :: String -> [SpringRows]
parseInput = fmap parseSpringRow . lines
  where
    parseSpringRow = (\[dr, dg] -> SR{damagedRecord = dr, damagedGroups = (fmap (\x -> read x :: Int) . splitOn ",") dg}) . words

fixRecords :: [String] -> String -> [String]
fixRecords rs ('?' : xs) =
    if null rs then fixRecords ["#", "."] xs else fixRecords (concatMap (\r -> [r ++ ['#'], r ++ ['.']]) rs) xs
fixRecords rs (x : xs) =
    if null rs then fixRecords [[x]] xs else fixRecords (fmap (\r -> r ++ [x]) rs) xs
fixRecords rs [] = rs

recordToGroup :: String -> [Int]
recordToGroup = fmap length . filter (not . null) . splitOn "."

howManyValidCombinations :: [Int] -> String -> Int
howManyValidCombinations expectedGroup = length . filter (== expectedGroup) . fmap recordToGroup . fixRecords []

solution :: [SpringRows] -> Int
solution = sum . fmap solutionSpringRow
  where
    solutionSpringRow (SR{damagedRecord = dr, damagedGroups = dg}) =
        howManyValidCombinations dg dr

testInput :: [SpringRows]
testInput =
    parseInput
        "???.### 1,1,3\n\
        \.??..??...?##. 1,1,3\n\
        \?#?#?#?#?#?#?#? 1,3,1,6\n\
        \????.#...#... 4,1,1\n\
        \????.######..#####. 1,6,5\n\
        \?###???????? 3,2,1"

twelfthDecemberSolution1 :: IO Int
twelfthDecemberSolution1 = solution <$> input

solution2 = undefined

twelfthDecemberSolution2 :: IO Int
twelfthDecemberSolution2 = undefined
