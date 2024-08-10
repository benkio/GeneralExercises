module TwentyTwentyThree.TwentyFifthDecember where

import Data.Map (Map, insertWith, empty)

import Data.List.Split (splitOn)

import Data.Bifunctor (bimap, first, second)

input :: IO (Map String [String])
input = parseInput <$> readFile "input/2023/25December.txt"

parseInput :: String -> Map String [String]
parseInput = foldl (\m (x, v) -> insertWith (++) x [v] m) empty .
  concatMap parseLineWire . lines
  where
    parseLineWire :: String -> [(String, String)]
    parseLineWire =
        (\(s, xs) -> concatMap (\x -> [(s, x), (x, s)]) xs) . second (splitOn " " . drop 2) . break (== ':')

testInput :: Map String [String]
testInput =
    parseInput
        "jqt: rhn xhk nvd\n\
        \rsh: frs pzl lsr\n\
        \xhk: hfx\n\
        \cmg: qnr nvd lhk bvb\n\
        \rhn: xhk bvb hfx\n\
        \bvb: xhk hfx\n\
        \pzl: lsr hfx nvd\n\
        \qnr: nvd\n\
        \ntq: jqt hfx bvb xhk\n\
        \nvd: lhk\n\
        \lsr: lhk\n\
        \rzs: qnr cmg lsr rsh\n\
        \frs: qnr lhk lsr"

solution1 = undefined

twentyfifthDecemberSolution1 :: IO Int
twentyfifthDecemberSolution1 = undefined

solution2 = undefined

twentyfifthDecemberSolution2 :: IO Int
twentyfifthDecemberSolution2 = undefined
