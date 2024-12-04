module TwentyTwentyFour.December04 where

import Data.List (transpose)
import Text.Regex.TDFA

type WordSearch = [String]

input :: IO String
input = readFile "input/2024/December04.txt"

testInput :: String
testInput =
    "MMMSXXMASM\n\
    \MSAMXMSMSA\n\
    \AMXSXMAAMM\n\
    \MSAMASMSMX\n\
    \XMASAMXAMM\n\
    \XXAMMXXAMA\n\
    \SMSMSASXSS\n\
    \SAXAMASAAA\n\
    \MAMMMXMMMM\n\
    \MXMXAXMASX\n"

wordSearchHorizontal, wordSearchVertical, wordSearchDiagonal :: String -> WordSearch
wordSearchHorizontal = lines
wordSearchVertical = transpose . lines
wordSearchDiagonal s =
    ((go . map reverse . init . reverse) straightLines)
        ++ go straightLines
        ++ ((go . map reverse . init . reverse) reverseLines)
        ++ go reverseLines
  where
    straightLines = lines s
    reverseLines = (map reverse . lines) s
    go [] = []
    go (line : lines) =
        zipWith (++) (map (: []) line ++ repeat []) ([] : go lines)

allWordSerach :: Int -> String -> WordSearch
allWordSerach targetWordLength s =
    (filter ((\x -> x >= targetWordLength) . length))
        =<< [ wordSearchHorizontal s
            , wordSearchVertical s
            , wordSearchDiagonal s
            ]

wordSearch :: String -> String -> Int
wordSearch target input =
    length (getAllTextMatches ((reverse input) =~ target) :: [String])
        + length (getAllTextMatches (input =~ target) :: [String])

solution1 :: String -> Int
solution1 s = foldl (\acc x -> acc + wordSearch "XMAS" x) 0 $ allWordSerach 4 s

december04Solution1 :: IO Int
december04Solution1 = solution1 <$> input

solution2 :: String -> Int
solution2 = undefined

december04Solution2 :: IO Int
december04Solution2 = solution2 <$> input
