module TwentyTwentyFour.December04 where

import Data.Map (Map, fromList, keys, (!?))
import qualified Data.Map as Map (filter)

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
    (go . map reverse . init . reverse) straightLines
        ++ go straightLines
        ++ (go . map reverse . init . reverse) reverseLines
        ++ go reverseLines
  where
    straightLines = lines s
    reverseLines = (map reverse . lines) s
    go [] = []
    go (line : lines) =
        zipWith (++) (map (: []) line ++ repeat []) ([] : go lines)

allWordSerach :: Int -> String -> WordSearch
allWordSerach targetWordLength s =
    filter ((>= targetWordLength) . length)
        =<< [ wordSearchHorizontal s
            , wordSearchVertical s
            , wordSearchDiagonal s
            ]

wordSearch :: String -> String -> Int
wordSearch target input =
    length (getAllTextMatches (reverse input =~ target) :: [String])
        + length (getAllTextMatches (input =~ target) :: [String])

solution1 :: String -> Int
solution1 s = foldl (\acc x -> acc + wordSearch "XMAS" x) 0 $ allWordSerach 4 s

december04Solution1 :: IO Int
december04Solution1 = solution1 <$> input

matrix :: String -> Map (Int, Int) Char
matrix =
    fromList . concatMap (\(y, s) -> zipWith (\x c -> ((x, y), c)) [0 ..] s) . zip [0 ..] . lines

findPatternCenterIndexes :: Map (Int, Int) Char -> [(Int, Int)]
findPatternCenterIndexes = keys . Map.filter (== 'A')

findPattern :: Map (Int, Int) Char -> (Int, Int) -> Bool
findPattern m (ax, ay) =
    checkCross topLeft bottomRight
        && checkCross topRight bottomLeft
  where
    topLeft = m !? (ax - 1, ay - 1)
    topRight = m !? (ax + 1, ay - 1)
    bottomLeft = m !? (ax - 1, ay + 1)
    bottomRight = m !? (ax + 1, ay + 1)
    checkCross (Just 'M') (Just 'S') = True
    checkCross (Just 'S') (Just 'M') = True
    checkCross _ _ = False

countPatterns :: Map (Int, Int) Char -> Int
countPatterns m = length . filter (findPattern m) $ findPatternCenterIndexes m

solution2 :: String -> Int
solution2 = countPatterns . matrix

december04Solution2 :: IO Int
december04Solution2 = solution2 <$> input
