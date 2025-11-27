module TwentyTwentyThree.December17 where

import Data.Bifunctor (first)
import Data.List (minimumBy)
import Data.Map (Map, empty, findMax, fromList, size, toList)
import Data.Map as Map (adjust, insert, insertWith, lookup, member)
import Data.Maybe (fromJust, isJust)
import Data.Set (Set, notMember)
import Data.Set as Set (insert)
import Debug.Trace
import Text.Printf (printf)

newtype HeatLossMap = HLM (Map (Int, Int) Int) deriving (Show)

data Direction = U | D | L | R deriving (Eq, Show, Ord)

data Node = N {coord :: (Int, Int), dir :: Direction, dirStreak :: Int} deriving (Show, Eq, Ord)

input :: IO HeatLossMap
input = parseInput <$> readFile "input/2023/17December.txt"

solution1 hlm = loop hlm (solution1AvailableDirectionsF 3) solution1EndCondition

-- solution 1256
december17Solution1 :: IO Int
december17Solution1 = solution1 <$> input

solution2 hlm = loop hlm solution2AvailableDirectionsF solution2EndCondition

december17Solution2 :: IO Int
december17Solution2 = solution2 <$> input

parseInput :: String -> HeatLossMap
parseInput input = HLM $ fromList $ concat $ zipWith parseRow [0 ..] (lines input)
  where
    parseRow :: Int -> String -> [((Int, Int), Int)]
    parseRow rowIndex = zipWith (\colIndex char -> ((colIndex, rowIndex), read [char])) [0 ..]

testInput :: HeatLossMap
testInput =
    parseInput
        "2413432311323\n\
        \3215453535623\n\
        \3255245654254\n\
        \3446585845452\n\
        \4546657867536\n\
        \1438598798454\n\
        \4457876987766\n\
        \3637877979653\n\
        \4654967986887\n\
        \4564679986453\n\
        \1224686865563\n\
        \2546548887735\n\
        \4322674655533"

testInput2 :: HeatLossMap
testInput2 =
    parseInput
        "111111111111\n\
        \999999999991\n\
        \999999999991\n\
        \999999999991\n\
        \999999999991"

initialNode = N{coord = (0, 0), dir = R, dirStreak = 0}

test1 = loop testInput (solution1AvailableDirectionsF 3) solution1EndCondition

test2a = loop testInput solution2AvailableDirectionsF solution2EndCondition

test2b = loop testInput2 solution2AvailableDirectionsF solution2EndCondition

loop :: HeatLossMap -> (Int -> Direction -> [Direction]) -> (Node -> (Int, Int) -> Bool) -> Int
loop grid@(HLM hlm) availableDirectiorF endConditionF =
    (snd . head . fst) $
        until
            ( \((n, v) : xs, s) ->
                if v `mod` 100 == 0
                    then
                        trace
                            (printf "debug: %s %s" (show (coord n, v)) (show (length xs, size s)))
                            endConditionF
                            n
                            maxCoord
                    else endConditionF n maxCoord
            )
            ( \(x : xs, s) ->
                let (xs', s') = nextNodes s grid x maxCoord availableDirectiorF
                 in (xs `mergeOrdered` xs', s')
            )
            ([(initialNode, 0)], empty)
  where
    maxCoord :: (Int, Int)
    maxCoord = (fst . findMax) hlm

solution1EndCondition :: Node -> (Int, Int) -> Bool
solution1EndCondition n end = coord n == end

solution2EndCondition :: Node -> (Int, Int) -> Bool
solution2EndCondition n end = solution1EndCondition n end && dirStreak n >= 4

solution1AvailableDirectionsF :: Int -> Int -> Direction -> [Direction]
solution1AvailableDirectionsF limit ds d =
    if ds == limit
        then case d of
            U -> [L, R]
            L -> [U, D]
            D -> [L, R]
            R -> [U, D]
        else case d of
            U -> [U, L, R]
            L -> [L, U, D]
            D -> [D, L, R]
            R -> [R, U, D]

solution2AvailableDirectionsF :: Int -> Direction -> [Direction]
solution2AvailableDirectionsF ds d
    | ds < 4 = [d]
    | otherwise = solution1AvailableDirectionsF 10 ds d

nextNodes :: Map Node Int -> HeatLossMap -> (Node, Int) -> (Int, Int) -> (Int -> Direction -> [Direction]) -> ([(Node, Int)], Map Node Int)
nextNodes visited (HLM m) (n@(N{coord = c, dir = d, dirStreak = ds}), v) (maxX, maxY) availableDirectiorF = (nextNodes, visited')
  where
    visited' = Map.insert n v visited
    availableDirections = availableDirectiorF ds d
    nextNodes :: [(Node, Int)]
    nextNodes =
        ( filter
            (\(node, nv) -> (fst . coord) node <= maxX && (fst . coord) node >= 0 && (snd . coord) node <= maxY && (snd . coord) node >= 0 && maybe True (nv <) (Map.lookup node visited'))
            . fmap
                ( \direction ->
                    let (c', d') = nextCoord c direction
                     in (N{coord = c', dir = d', dirStreak = if d' == d then ds + 1 else 1}, v + fromJust (Map.lookup c' m))
                )
        )
            availableDirections
    nextCoord :: (Int, Int) -> Direction -> ((Int, Int), Direction)
    nextCoord (x, y) U = ((x, y - 1), U)
    nextCoord (x, y) D = ((x, y + 1), D)
    nextCoord (x, y) L = ((x - 1, y), L)
    nextCoord (x, y) R = ((x + 1, y), R)

mergeOrdered :: (Eq a) => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
mergeOrdered xs [] = xs
mergeOrdered [] ys = ys
mergeOrdered ((x, xv) : xs) ((y, yv) : ys)
    | x /= y && xv <= yv = (x, xv) : mergeOrdered xs ((y, yv) : ys)
    | x /= y && xv > yv = (y, yv) : mergeOrdered ((x, xv) : xs) ys
    | x == y && xv < yv = (x, xv) : mergeOrdered xs ys
    | x == y && xv >= yv = (y, yv) : mergeOrdered xs ys
