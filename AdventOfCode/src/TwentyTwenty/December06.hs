-------------------------------------------------------------------------------
--                           Advent Of Code - day 6                          --
-------------------------------------------------------------------------------
module TwentyTwenty.December06 where

import Data.List (groupBy, intersect, nub)

splitByGroup :: [String] -> [[String]]
splitByGroup = fmap (filter ("" /=)) . groupBy (\_ x -> x /= "")

countTotalAnyoneYesAnswers :: [[String]] -> Int
countTotalAnyoneYesAnswers = sum . fmap (length . nub . concat)

countTotalEveryoneYesAnswers :: [[String]] -> Int
countTotalEveryoneYesAnswers =
    sum . fmap (length . foldl1 intersect) . filter (not . null)

input :: IO [[String]]
input = splitByGroup . lines <$> readFile "input/2020/6December.txt"

december06Solution1 :: IO Int
december06Solution1 = countTotalAnyoneYesAnswers <$> input

december06Solution2 :: IO Int
december06Solution2 = countTotalEveryoneYesAnswers <$> input
