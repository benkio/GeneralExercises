module TwentyTwentyTwo.December03 where

import Data.List (find, intersect)
import Data.Maybe (fromJust)

data Rucksack = Rucksack
    { first :: String
    , second :: String
    }
    deriving (Show)

parseInput :: [String] -> [Rucksack]
parseInput = fmap (\x -> Rucksack{first = take (length x `div` 2) x, second = drop (length x `div` 2) x})

input :: IO [Rucksack]
input = parseInput . lines <$> readFile "input/2022/3December.txt"

testInput :: [Rucksack]
testInput =
    (parseInput . lines)
        "vJrwpWtwJgWrhcsFMMfFFhFp\n\
        \jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
        \PmmdzqPrVvPwwTWBwg\n\
        \wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
        \ttgJtRGJQctTZtZT\n\
        \CrZsJsPPZsGzwwsLwLmpwMDw"

rucksackCompartmentsIntersection :: Rucksack -> Char
rucksackCompartmentsIntersection Rucksack{first = f, second = s} = head $ intersect f s

priorityMap :: [(Char, Int)]
priorityMap = (['a' .. 'z'] ++ ['A' .. 'Z']) `zip` [1 .. 52]

itemPriority :: Char -> Int
itemPriority c = snd $ fromJust $ find (\(c', _) -> c' == c) priorityMap

solution1 :: [Rucksack] -> Int
solution1 = sum . fmap (itemPriority . rucksackCompartmentsIntersection)

december03Solution1 :: IO Int
december03Solution1 = solution1 <$> input

groupElfs :: [Rucksack] -> [[Rucksack]]
groupElfs xs =
    let (groups, lastGroup) = foldl (\(acc, lacc) x -> if length lacc == 3 then (acc ++ [lacc], [x]) else (acc, lacc ++ [x])) ([], []) xs
     in groups ++ [lastGroup]

elfsBadges :: [[Rucksack]] -> String
elfsBadges = fmap compareRucksacks
  where
    compareRucksacks :: [Rucksack] -> Char
    compareRucksacks [] = error "unexpected empty list"
    compareRucksacks xs = head $ foldl1 intersect $ fmap (\r -> first r ++ second r) xs

solution2 :: [Rucksack] -> Int
solution2 = sum . fmap itemPriority . elfsBadges . groupElfs

december03Solution2 :: IO Int
december03Solution2 = solution2 <$> input
