module TwentyTwentyFour.December20 where

import Data.Bifunctor (bimap, first, second)

import Data.List (minimumBy)
import Data.Map (Map, fromList, insert, toList, (!?))
import Debug.Trace
import Lib.Coord (Coord, coordDistance)
import Lib.CoordMap (findBranches, findBridges)
import Lib.Direction (Direction (..))
import Lib.Parse (parseGridWithElemSelection)
import Lib.Pathfinding (Node (..), defaultMapToPaths, minimumSteps, pathToCoord)
import Lib.Print (printGridMapDefault)

data ChipField = S | E | Empty deriving (Eq, Ord)
type ChipMap = Map Coord ChipField

instance Show ChipField where
    show S = "S"
    show E = "E"
    show Empty = "."

input :: IO (ChipMap, [(Coord, ChipField)])
input = parseInput <$> readFile "input/2024/December20.txt"

parseInput :: String -> (ChipMap, [(Coord, ChipField)])
parseInput =
    (\(msl, points) -> (fromList (msl ++ fmap (second (const Empty)) points), points))
        . parseGridWithElemSelection
            ( \y x c -> case c of
                '#' -> Nothing
                '.' -> Just (Left ((x, y), Empty))
                'S' -> Just (Right ((x, y), S))
                'E' -> Just (Right ((x, y), E))
            )

printInput :: (ChipMap, [(Coord, ChipField)]) -> IO ()
printInput = putStrLn . printGridMapDefault . fst
printTestInput = printInput testInput
printActualInput = printInput =<< input

startPoint, endPoint :: (ChipMap, [(Coord, ChipField)]) -> (Coord, ChipField)
startPoint (_, [s, _]) = s
endPoint (_, [_, e]) = e

insertBridges :: Map Coord ChipField -> [Map Coord ChipField]
insertBridges ms = fmap (\c -> insert c Empty ms) . findBridges $ ms

paths :: (Coord, ChipField) -> Coord -> Map Coord ChipField -> [[Node ChipField]]
paths start endCoord = defaultMapToPaths start endCoord

solution1 :: Int -> (ChipMap, [(Coord, ChipField)]) -> Int
solution1 p i =
    length
        . filter (>= p)
        . fmap ((\x -> standardScore - x) . fst . traceShowId . minimumSteps . paths (startPoint i) ((fst . endPoint) i))
        . insertBridges
        $ ms
  where
    (ms, relevantPoints) = i
    sp = startPoint i
    (ec, _) = endPoint i
    (standardScore, _) = minimumSteps $ paths sp ec ms

test i = pathScores i

pathScores :: (ChipMap, [(Coord, ChipField)]) -> Map Coord (Int,Int)
pathScores i@(ms,_) = fromList . fmap (\(i, c) -> (c, (i,length psExpanded - i))) $ zip [0..] psExpanded
  where
    sp = startPoint i
    (ec, _) = endPoint i
    (_,ps) = minimumSteps $ paths sp ec ms
    endCondition c _ = c == ec
    psExpanded = pathToCoord ps ms endCondition


december20Solution1 :: IO Int
december20Solution1 = solution1 100 <$> input

solution2 :: (ChipMap, [(Coord, ChipField)]) -> Int
solution2 = undefined

december20Solution2 :: IO Int
december20Solution2 = solution2 <$> input

testInput :: (ChipMap, [(Coord, ChipField)])
testInput =
    parseInput
        "###############\n\
        \#...#...#.....#\n\
        \#.#.#.#.#.###.#\n\
        \#S#...#.#.#...#\n\
        \#######.#.#.###\n\
        \#######.#.#...#\n\
        \#######.#.###.#\n\
        \###..E#...#...#\n\
        \###.#######.###\n\
        \#...###...#...#\n\
        \#.#####.#.###.#\n\
        \#.#...#.#.#...#\n\
        \#.#.#.#.#.#.###\n\
        \#...#...#...###\n\
        \###############\n"
