{-# LANGUAGE TupleSections #-}

module TwentyTwentyThree.December11 where

import Data.Bifunctor (second)
import Data.List (transpose)

data Sky = E | G deriving (Eq)

instance Show Sky where
    show E = "."
    show G = "#"

instance Read Sky where
    readsPrec _ (c : s) =
        case c of
            '.' -> [(E, s)]
            '#' -> [(G, s)]
            _ -> []

input :: IO [[Sky]]
input = parseInput <$> readFile "input/2023/11December.txt"

parseInput :: String -> [[Sky]]
parseInput = (fmap . fmap) (\x -> read [x] :: Sky) . lines

expandUniverse :: [[Sky]] -> ([Int], [Int])
expandUniverse xs = (emptyLines 0 xs, (emptyLines 0 . transpose) xs)

emptyLines :: Int -> [[Sky]] -> [Int]
emptyLines _ [] = []
emptyLines i (x : xs)
    | all (== E) x = i : emptyLines (i + 1) xs
    | otherwise = emptyLines (i + 1) xs

galaxyCoordinates :: [[Sky]] -> [(Int, Int)]
galaxyCoordinates = foldl (\gs (y, r) -> foldl (\gs' (x, sky) -> if sky == G then (x, y) : gs' else gs') gs r) [] . fmap (second (zip [0 ..])) . zip [0 ..]

galaxyPairs :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
galaxyPairs [] = []
galaxyPairs (g : gs) = fmap (g,) gs ++ galaxyPairs gs

manhattanDistance :: [Int] -> [Int] -> Int -> (Int, Int) -> (Int, Int) -> Int
manhattanDistance rs cs expRate (x, y) (x', y') =
    abs (x - x')
        + abs (y - y')
        + (enclosedRows * (expRate - 1))
        + (enclosedColumns * (expRate - 1))
  where
    enclosedRows = (length . filter (\z -> z < max y y' && z > min y y')) rs
    enclosedColumns = (length . filter (\z -> z < max x x' && z > min x x')) cs

testInput :: [[Sky]]
testInput =
    parseInput
        "...#......\n\
        \.......#..\n\
        \#.........\n\
        \..........\n\
        \......#...\n\
        \.#........\n\
        \.........#\n\
        \..........\n\
        \.......#..\n\
        \#...#....."

solution expRate xs = (sum . fmap (uncurry (manhattanDistance emptyRows emptyColumns expRate)) . galaxyPairs . galaxyCoordinates) xs
  where
    (emptyRows, emptyColumns) = expandUniverse xs

december11Solution1 :: IO Int
december11Solution1 = solution 2 <$> input

december11Solution2 :: IO Int
december11Solution2 = solution 1000000 <$> input
