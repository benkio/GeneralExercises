module TwentyTwentyThree.EighteenthDecember where

import Data.List (groupBy, maximumBy, minimumBy)
import Data.Set (Set, empty, insert, size)

data Direction = U | D | L | R deriving (Show)
data DigPlan = DP {direction :: Direction, steps :: Int, color :: String} deriving (Show)
type EdgeBlock = ((Int, Int), Direction)

parseInput :: String -> [DigPlan]
parseInput = fmap ((\[d, s, c] -> DP{direction = parseDirection d, steps = (read s :: Int), color = (take 6 . drop 2) c}) . words) . lines
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

digPlanToEdges :: [DigPlan] -> [EdgeBlock]
digPlanToEdges = snd . foldl followDigPlan ((0, 0), [])
  where
    followDigPlan :: ((Int, Int), [EdgeBlock]) -> DigPlan -> ((Int, Int), [EdgeBlock])
    followDigPlan (pos, acc) digPlan =
        let edge = createEdge pos digPlan in ((fst . last) edge, acc ++ edge)
    createEdge :: (Int, Int) -> DigPlan -> [EdgeBlock]
    createEdge (x, y) (DP{direction = U, steps = s}) = [((x, b), U) | b <- fmap (y -) [1 .. s]]
    createEdge (x, y) (DP{direction = D, steps = s}) = [((x, b), D) | b <- fmap (y +) [1 .. s]]
    createEdge (x, y) (DP{direction = R, steps = s}) = [((a, y), R) | a <- fmap (x +) [1 .. s]]
    createEdge (x, y) (DP{direction = L, steps = s}) = [((a, y), L) | a <- fmap (x -) [1 .. s]]

findInsideLagoon :: [EdgeBlock] -> Int
findInsideLagoon es = go es es empty
  where
    go :: [EdgeBlock] -> [EdgeBlock] -> Set (Int, Int) -> Int
    go [] _ lagoon = size lagoon
    go ((c, d) : xs) es lagoon =
        let
            c' = move c (inDirections d)
            inside = calcInsideBlocks c' (inDirections d) (fmap fst es)
         in
            go xs es (foldl (flip insert) lagoon inside)

calcInsideBlocks :: (Int, Int) -> Direction -> [(Int,Int)] -> [(Int, Int)]
calcInsideBlocks c ind es
    | c `elem` es = []
    | c `notElem` es = c : calcInsideBlocks (move c ind) ind es

move :: (Int, Int) -> Direction -> (Int, Int)
move (x, y) U = (x, y - 1)
move (x, y) D = (x, y + 1)
move (x, y) R = (x + 1, y)
move (x, y) L = (x - 1, y)

inDirections :: Direction -> Direction
inDirections R = D
inDirections D = L
inDirections L = U
inDirections U = R

solution1 :: [DigPlan] -> Int
solution1 digPlan = length es + inside
  where
    es = digPlanToEdges digPlan
    inside = findInsideLagoon es

-- too low 49570
eighteenthDecemberSolution1 :: IO Int
eighteenthDecemberSolution1 = solution1 <$> input

eighteenthDecemberSolution2 :: IO Int
eighteenthDecemberSolution2 = undefined

solution2 = undefined
