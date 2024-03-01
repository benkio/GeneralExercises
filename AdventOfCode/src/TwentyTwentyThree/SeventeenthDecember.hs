module TwentyTwentyThree.SeventeenthDecember where

import Text.Printf (printf)

import Data.List (delete)
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

-- solution1 hlm = loop (initialNodeWeigth hlm) hlm [((0,0), [])]

seventeenthDecemberSolution1 :: IO Int
seventeenthDecemberSolution1 = undefined

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

test n = nextNodes empty testInput n 3

loop :: Map Node Int -> HeatLossMap -> [(Node, Int)] -> Int
loop _ _ [] = error "no more nodes!"
loop m hlm ((n, v):ns) = undefined

-- loop grid =
--     until
--         ( \(xs, s) ->
--             -- trace (printf "debug: %s" (show (length xs, size s)))
--             (null xs)
--         )
--         ( \((x : xs), s) ->
--             let (xs', s') = nextNodes s grid x 3
--              in ( filter (\(n, nv) -> maybe True (\onv -> nv < onv) (Map.lookup n s')) (xs ++ xs')
--                 , s'
--                 )
--         )
--         ([(initialNode, 0)], empty)

nextNodes :: Map Node Int -> HeatLossMap -> (Node, Int) -> Int -> ([(Node, Int)], Map Node Int)
nextNodes visited (HLM m) (n@(N{coord = c, dir = d, dirStreak = ds}), v) pathLimit = (nextNodes, Map.insertWith min n v visited)
  where
    ((maxX, maxY), _) = findMax m
    availableDirections =
        if ds == pathLimit
            then d `delete` [U, D, L, R]
            else [U, D, L, R]
    nextNodes :: [(Node, Int)]
    nextNodes =
        ( filter
            ( \(node, nv) -> dirStreak node <= pathLimit)
            . fmap
                ( \(c', d') ->
                    (N{coord = c', dir = d', dirStreak = if d' == d then ds + 1 else 1}, (v + (fromJust (Map.lookup c' m))))
                )
            . filter (\((x, y), _) -> x <= maxX && x >= 0 && y <= maxY && y >= 0)
            . fmap (nextCoord c)
        )
            availableDirections
    nextCoord :: (Int, Int) -> Direction -> ((Int, Int), Direction)
    nextCoord (x, y) U = ((x, y - 1), U)
    nextCoord (x, y) D = ((x, y + 1), D)
    nextCoord (x, y) L = ((x - 1, y), L)
    nextCoord (x, y) R = ((x + 1, y), R)
