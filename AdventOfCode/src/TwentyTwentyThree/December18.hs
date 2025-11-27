module TwentyTwentyThree.December18 where

import Data.Char (digitToInt)
import Data.List (groupBy, maximumBy, minimumBy)
import Data.Set (Set, empty, insert, size)
import Debug.Trace
import Numeric (readHex)
import Text.Printf (printf)

data Direction = R | D | L | U deriving (Show, Enum)

data DigPlan = DP {direction :: Direction, steps :: Int, color :: String} deriving (Show)

type EdgeBlock = ((Int, Int), Direction)

parseInput :: String -> [DigPlan]
parseInput = fmap ((\[d, s, c] -> DP{direction = parseDirection d, steps = read s :: Int, color = (take 6 . drop 2) c}) . words) . lines
  where
    parseDirection "U" = U
    parseDirection "L" = L
    parseDirection "R" = R
    parseDirection "D" = D

input :: IO [DigPlan]
input = parseInput <$> readFile "input/2023/18December.txt"

testInput :: [DigPlan]
testInput =
    parseInput
        "R 6 (#70c710)\n\
        \D 5 (#0dc571)\n\
        \L 2 (#5713f0)\n\
        \D 2 (#d2c081)\n\
        \R 2 (#59c680)\n\
        \D 2 (#411b91)\n\
        \L 5 (#8ceee2)\n\
        \U 2 (#caa173)\n\
        \L 1 (#1b58a2)\n\
        \U 2 (#caa171)\n\
        \R 2 (#7807d2)\n\
        \U 3 (#a77fa3)\n\
        \L 2 (#015232)\n\
        \U 2 (#7a21e3)"

area :: [DigPlan] -> Int
area = go (0, 0)
  where
    go :: (Int, Int) -> [DigPlan] -> Int
    go _ [] = 1
    go (x, y) (DP{direction = R, steps = s} : ds) = s + go (x + s, y) ds
    go (x, y) (DP{direction = D, steps = s} : ds) = (x + 1) * s + go (x, y + s) ds
    go (x, y) (DP{direction = U, steps = s} : ds) = (-(x * s)) + go (x, y - s) ds
    go (x, y) (DP{direction = L, steps = s} : ds) = go (x - s, y) ds

solution1 :: [DigPlan] -> Int
solution1 = area

-- too low 49570
december18Solution1 :: IO Int
december18Solution1 = solution1 <$> input

december18Solution2 :: IO Int
december18Solution2 = solution2 <$> input

digPlanHex :: [DigPlan] -> [DigPlan]
digPlanHex [] = []
digPlanHex (DP{color = c} : ds) =
    ( (\[(h, "")] -> DP{direction = (toEnum . digitToInt . last) c, steps = h, color = c})
        . readHex
        . init
    )
        c
        : digPlanHex ds

solution2 = area . digPlanHex
