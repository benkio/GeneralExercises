-------------------------------------------------------------------------------
--                           Advent Of Code - day 6                          --
-------------------------------------------------------------------------------
module SixthDecember where

import Data.List (groupBy, intersect, nub)

questionIds :: [Char]
questionIds = ['a' .. 'z']

splitByGroup :: [String] -> [[String]]
splitByGroup = fmap (filter ("" /=)) . groupBy (\_ x -> x /= "")

countTotalAnyoneYesAnswers :: [[String]] -> Int
countTotalAnyoneYesAnswers = sum . fmap (length . nub . concat)

countTotalEveryoneYesAnswers :: [[String]] -> Int
countTotalEveryoneYesAnswers =
  sum . fmap (length . foldl1 intersect) . filter (not . null)

input :: IO [[String]]
input = splitByGroup . lines <$> readFile "input/6December.txt"

sixthDecemberSolution1 :: IO Int
sixthDecemberSolution1 = countTotalAnyoneYesAnswers <$> input

sixthDecemberSolution2 :: IO Int
sixthDecemberSolution2 = countTotalEveryoneYesAnswers <$> input
