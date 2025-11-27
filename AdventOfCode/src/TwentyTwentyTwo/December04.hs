module TwentyTwentyTwo.December04 where

import Data.Bifunctor (bimap)
import Data.List (isSubsequenceOf)

data Assignment = Assignment
    { first :: [Int]
    , second :: [Int]
    }
    deriving (Show)

input :: IO [Assignment]
input = parseInput . lines <$> readFile "input/2022/4December.txt"

parseInput :: [String] -> [Assignment]
parseInput = fmap parseAssignemnt
  where
    parseAssignemnt :: String -> Assignment
    parseAssignemnt = (\(r1, r2) -> Assignment{first = r1, second = r2}) . bimap stringToRange (stringToRange . tail) . break (== ',')
    stringToRange :: String -> [Int]
    stringToRange = uncurry enumFromTo . bimap (\x -> read x :: Int) (\x -> read (tail x) :: Int) . break (== '-')

testInput :: [Assignment]
testInput =
    (parseInput . lines)
        "2-4,6-8\n\
        \2-3,4-5\n\
        \5-7,7-9\n\
        \2-8,3-7\n\
        \6-6,4-6\n\
        \2-6,4-8"

assignmentContained :: Assignment -> Bool
assignmentContained Assignment{first = f, second = s} = isSubsequenceOf f s || isSubsequenceOf s f

solution1 :: [Assignment] -> Int
solution1 = length . filter id . fmap assignmentContained

december04Solution1 :: IO Int
december04Solution1 = solution1 <$> input

assignmentOverlap :: Assignment -> Bool
assignmentOverlap Assignment{first = f, second = s} = any (`elem` s) f

solution2 :: [Assignment] -> Int
solution2 = length . filter id . fmap assignmentOverlap

december04Solution2 :: IO Int
december04Solution2 = solution2 <$> input
