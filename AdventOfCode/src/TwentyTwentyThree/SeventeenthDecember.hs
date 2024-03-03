module TwentyTwentyThree.SeventeenthDecember where

import Data.Bifunctor (first)
import Text.Printf (printf)

import Data.Map (Map, empty, findMax, fromList, size)
import Data.Map as Map (adjust, insert, insertWith, lookup, member)
import Data.Maybe (fromJust, isJust)
import Data.Set (Set, notMember)
import Data.Set as Set (insert)
import Debug.Trace

newtype HeatLossMap = HLM (Map (Int, Int) Int) deriving (Show)
data Direction = U | D | L | R deriving (Eq, Show, Ord)
data Node = N {coord :: (Int, Int), dir :: Direction, dirStreak :: Int} deriving (Show, Eq, Ord)

input :: IO HeatLossMap
input = parseInput <$> readFile "input/2023/17December.txt"

solution1 hlm = loop hlm

-- solution 1256
seventeenthDecemberSolution1 :: IO Int
seventeenthDecemberSolution1 = solution1 <$> input

solution2 = undefined

seventeenthDecemberSolution2 :: IO Int
seventeenthDecemberSolution2 = undefined

parseInput :: String -> HeatLossMap
parseInput input = HLM $ fromList $ concat $ zipWith (\rowIndex row -> parseRow rowIndex row) [0 ..] (lines input)
  where
    parseRow :: Int -> String -> [((Int, Int), Int)]
    parseRow rowIndex row = zipWith (\colIndex char -> ((colIndex, rowIndex), read [char])) [0 ..] row

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

initialNode = N{coord = (0, 0), dir = R, dirStreak = 0}

test = loop testInput

loop :: HeatLossMap -> Int
loop grid@(HLM hlm) =
    (snd . head . fst) $
        until
            ( \(((n, v) : xs), s) ->
                if v `mod` 100 == 0
                    then
                        trace
                            (printf "debug: %s %s" (show (coord n, v)) (show (length xs, size s)))
                            (coord n == maxCoord)
                    else (coord n == maxCoord)
            )
            ( \((x : xs), s) ->
                let (xs', s') = nextNodes s grid x 3 maxCoord
                 in ((xs `mergeOrdered` xs'), s')
            )
            ([(initialNode, 0)], empty)
  where
    maxCoord :: (Int, Int)
    maxCoord = (fst . findMax) hlm

nextNodes :: Map Node Int -> HeatLossMap -> (Node, Int) -> Int -> (Int, Int) -> ([(Node, Int)], Map Node Int)
nextNodes visited (HLM m) (n@(N{coord = c, dir = d, dirStreak = ds}), v) pathLimit (maxX, maxY) = (nextNodes, visited')
  where
    visited' = Map.insertWith min n v visited
    availableDirections =
        if ds == pathLimit
            then case d of
                U -> [D, L, R]
                L -> [U, D, R]
                D -> [U, L, R]
                R -> [U, L, D]
            else [U, D, L, R]
    nextNodes :: [(Node, Int)]
    nextNodes =
        ( filter
            (\(node, nv) -> ((fst . coord) node) <= maxX && ((fst . coord) node) >= 0 && ((snd . coord) node) <= maxY && ((snd . coord) node) >= 0 && maybe True (\onv -> nv < onv) (Map.lookup node visited'))
            . fmap
                ( \direction ->
                    let (c', d') = nextCoord c direction
                     in (N{coord = c', dir = d', dirStreak = if d' == d then ds + 1 else 1}, (v + (fromJust (Map.lookup c' m))))
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
