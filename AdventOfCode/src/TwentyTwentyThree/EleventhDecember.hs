{-# LANGUAGE TupleSections #-}

module TwentyTwentyThree.EleventhDecember where

import Data.Bifunctor (second)
import Data.List (transpose)

data Sky = E | G deriving (Eq)

instance Show Sky where
    show E = "."
    show G = "#"

instance Read Sky where
    readsPrec _ = \(c : s) -> case c of
        '.' -> [(E, s)]
        '#' -> [(G, s)]
        otherwise -> []

input :: IO [[Sky]]
input = parseInput <$> readFile "input/2023/11December.txt"

parseInput :: String -> [[Sky]]
parseInput = (fmap . fmap) (\x -> (read [x]) :: Sky) . lines

expandUniverse :: Int -> [[Sky]] -> [[Sky]]
expandUniverse expRate = transpose . expandUniverseRow expRate . transpose . expandUniverseRow expRate

expandUniverseRow :: Int -> [[Sky]] -> [[Sky]]
expandUniverseRow _ [] = []
expandUniverseRow expRate (x : xs)
    | all (== E) x = replicate expRate x ++ expandUniverseRow expRate xs
    | otherwise = x : expandUniverseRow expRate xs

galaxyCoordinates :: [[Sky]] -> [(Int, Int)]
galaxyCoordinates = foldl (\gs (y, r) -> foldl (\gs' (x, sky) -> if sky == G then (x, y) : gs' else gs') gs r) [] . fmap (second (zip [0 ..])) . zip [0 ..]

galaxyPairs :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
galaxyPairs [] = []
galaxyPairs (g : gs) = fmap (g,) gs ++ galaxyPairs gs

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x, y) (x', y') = abs (x - x') + abs (y - y')

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

solution1 expRate = sum . fmap (uncurry manhattanDistance) . galaxyPairs . galaxyCoordinates . expandUniverse expRate

eleventhDecemberSolution1 :: IO Int
eleventhDecemberSolution1 = solution1 2 <$> input

solution2 = undefined

eleventhDecemberSolution2 :: IO Int
eleventhDecemberSolution2 = solution1 1000000 <$> input
