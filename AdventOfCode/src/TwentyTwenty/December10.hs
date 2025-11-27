-------------------------------------------------------------------------------
--                           Advent Of Code - day 10                          --
-------------------------------------------------------------------------------
module TwentyTwenty.December10 where

import Data.List (sort, transpose)

input :: IO [Int]
input =
    fmap (\x -> read x :: Int) . lines <$> readFile "input/2020/10December.txt"

joltDifferences :: [Int] -> [Int]
joltDifferences js =
    let joltSorted = sort (0 : js)
     in fmap (\(x, y) -> y - x) $ joltSorted `zip` tail joltSorted

countJoltDifferences :: Int -> [Int] -> Int
countJoltDifferences differenceToCount = length . filter (differenceToCount ==)

arrangementsBySequentValues :: [Int]
arrangementsBySequentValues =
    1
        : 1
        : 2
        : fmap
            sum
            ( transpose
                [ arrangementsBySequentValues
                , tail arrangementsBySequentValues
                , (tail . tail) arrangementsBySequentValues
                ]
            )

groupByDifferences :: [Int] -> [[Int]]
groupByDifferences xs = foldl go [[]] joltDifferencesZip
  where
    joltDifferencesZip = sort xs `zip` joltDifferences xs
    go :: [[Int]] -> (Int, Int) -> [[Int]]
    go ls (x, diff)
        | diff == 3 = ls ++ [[x]]
        | otherwise = init ls ++ [last ls ++ [x]]

december10Solution1 :: IO Int
december10Solution1 = do
    jolts <- input
    let jDiff = joltDifferences jolts
        jDiff1 = countJoltDifferences 1 jDiff
        jDiff3 = countJoltDifferences 3 jDiff
    return $ jDiff1 * (jDiff3 + 1)

december10Solution2 :: IO Int
december10Solution2 =
    product
        . fmap (fst . last . (arrangementsBySequentValues `zip`))
        . groupByDifferences
        . (0 :)
        <$> input
