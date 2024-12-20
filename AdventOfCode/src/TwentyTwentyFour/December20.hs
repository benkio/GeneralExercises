module TwentyTwentyFour.December20 where

import Data.Bifunctor (bimap, first, second)

import Data.Functor ((<&>))
import Data.List (minimumBy)
import Data.Map (Map, fromList, insert, toList, (!?))
import Lib.Coord (Coord, manhattanDistance')
import Lib.CoordMap (findBranches, findBridges)
import Lib.Direction (Direction (..))
import Lib.List (pairs)
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
insertBridges ms = fmap (foldl (\m c -> insert c Empty m) ms) . findBridges 2 $ ms

paths :: (Coord, ChipField) -> Coord -> Map Coord ChipField -> [[Node ChipField]]
paths = defaultMapToPaths

solution1 :: Int -> (ChipMap, [(Coord, ChipField)]) -> Int
solution1 p i =
    length
        . filter (>= p)
        . fmap ((standardScore -) . fst . minimumSteps . paths (startPoint i) ((fst . endPoint) i))
        . insertBridges
        $ ms
  where
    (ms, relevantPoints) = i
    sp = startPoint i
    (ec, _) = endPoint i
    (standardScore, _) = minimumSteps $ paths sp ec ms

solution1' :: [Int] -> Int -> (ChipMap, [(Coord, ChipField)]) -> Int
solution1' bridgeLengths p i =
    length
        . filter (>= p)
        . fmap ((standardScore -) . pathScoreByBridge i standardPathScores)
        $ bridges
  where
    (ms, _) = i
    sp = startPoint i
    (ec, _) = endPoint i
    (standardScore, _) = minimumSteps $ paths sp ec ms
    bridges = concatMap (`findBridges` ms) bridgeLengths
    standardPathScores = fromList $ pathScores i

findShortcuts :: Int -> Int -> [(Coord, (Int, Int))] -> Int
findShortcuts bridgeLength minimalGain path = length $ fmap (uncurry calculatePathScoreWithBridge) valuableShortcuts
  where
    pathPairs = pairs path
    standardScore :: Int
    standardScore = (fst . snd . last) path
    valuableShortcuts :: [((Coord, (Int, Int)), (Coord, (Int, Int)))]
    valuableShortcuts = filter (uncurry goodShortcut) pathPairs
    goodShortcut :: (Coord, (Int, Int)) -> (Coord, (Int, Int)) -> Bool
    goodShortcut x y =
        manhattanDistance' (fst x) (fst y) <= bridgeLength
            && calculatePathScoreWithBridge x y <= (standardScore - minimalGain)

calculatePathScoreWithBridge (c, (s, e)) (c', (s', e')) = min s s' + manhattanDistance' c c' + min e e'

pathScores :: (ChipMap, [(Coord, ChipField)]) -> [(Coord, (Int, Int))]
pathScores i@(ms, _) =
    (\(i, c) -> (c, (i + 1, length psExpanded - 1 - i)))
        <$> zip [0 ..] psExpanded
  where
    sp = startPoint i
    (ec, _) = endPoint i
    (_, ps) = minimumSteps $ paths sp ec ms
    endCondition c _ = c == ec
    psExpanded = pathToCoord ps ms endCondition

pathScoreByBridge :: (ChipMap, [(Coord, ChipField)]) -> Map Coord (Int, Int) -> [Coord] -> Int
pathScoreByBridge i standardPathScores bridge = maybe (normalPathCalculation bridge) (uncurry (bridgePath bridge)) knownBridgeEndings
  where
    (ms, _) = i
    startBridge = head bridge
    endBridge = last bridge
    bridgePath b (fromStart, toEnd) (fromStart', toEnd') = min fromStart fromStart' + (length b - 1) + min toEnd toEnd'
    knownBridgeEndings = do
        s <- standardPathScores !? startBridge
        e <- standardPathScores !? endBridge
        return (s, e)
    normalPathCalculation =
        fst
            . minimumSteps
            . paths (startPoint i) ((fst . endPoint) i)
            . foldl (\m c -> insert c Empty m) ms

december20Solution1 :: IO Int
december20Solution1 = solution1' [2] 100 <$> input

solution2 :: Int -> Int -> (ChipMap, [(Coord, ChipField)]) -> Int
solution2 bridgeLength gain i = findShortcuts bridgeLength gain (pathScores i)

-- too low 120402
-- too low 982168
december20Solution2 :: IO Int
december20Solution2 = solution2 20 100 <$> input

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
