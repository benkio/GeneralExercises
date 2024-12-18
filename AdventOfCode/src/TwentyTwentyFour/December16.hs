module TwentyTwentyFour.December16 where

import Control.Arrow
import Data.Bifunctor (bimap)
import Data.List (find, groupBy, minimumBy, nub, sortBy)
import Data.Map (Map)
import Data.Map as Map (fromList, lookup, toList)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import qualified Data.Set as S (fromList, size)
import Data.Tree (Tree, drawTree, foldTree)
import Data.Void
import Debug.Trace
import Lib.Coord (Coord, coordDistance, findCardinalNeighboors)
import Lib.CoordMap (findBranches)
import Lib.Direction (Direction (..), turnsToDirection)
import Lib.Parse (parseGridWithElemSelection)
import Lib.Pathfinding (Node (..), mapToPaths, pathToCoord)

data Terrain = S | E | T deriving (Show, Eq)
type ReindeerMap = Map Coord Terrain

input :: IO ReindeerMap
input = parseInput <$> readFile "input/2024/December16.txt"

parseInput :: String -> ReindeerMap
parseInput = fromList . fst . parseGridWithElemSelection parseReindeerMap
  where
    parseReindeerMap :: Int -> Int -> Char -> Maybe (Either (Coord, Terrain) Void)
    parseReindeerMap y x '#' = Nothing
    parseReindeerMap y x '.' = Just $ Left ((x, y), T)
    parseReindeerMap y x 'S' = Just $ Left ((x, y), S)
    parseReindeerMap y x 'E' = Just $ Left ((x, y), E)

detectEnd _ endValue = endValue == E
filterNode currentScore prevScore = prevScore + 1000 < currentScore
filterNextNode nextScore prevScore = nextScore < (prevScore + 1000)
sortNodesF (_,score) = score

buildPaths :: (Coord, Terrain) -> Coord -> ReindeerMap -> [[Node Terrain]]
buildPaths startingPoint target = mapToPaths startingPoint East detectEnd calculateScore filterNode filterNextNode sortNodesF

findPoint :: Terrain -> ReindeerMap -> (Coord, Terrain)
findPoint t = fromJust . find ((== t) . snd) . toList

-- foldTree :: (a -> [b] -> b) -> Tree a -> b
findBestPathScore :: Tree (Node Terrain, Int) -> (Int, [Coord])
findBestPathScore = foldTree foldNCalculateScore

foldNCalculateScore :: (Node Terrain, Int) -> [(Int, [Coord])] -> (Int, [Coord])
foldNCalculateScore (n, v) xs
    | val n == E = trace ("calculate on End " ++ show v) (v, [nc n])
    | null candidates = trace ("discard node " ++ show (nc n)) (-1, [])
    | otherwise = trace ("calculate on node " ++ show (nc n)) $ minimumBy (comparing fst) candidates
  where
    candidates = filter ((>= 0) . fst) xs

calculateScore :: Node Terrain -> Int
calculateScore (N{distanceFromParent = dist, turnL = tl, turnR = tr}) = dist + (1000 * (tl + tr))

calculateScoreToNorth :: Node Terrain -> Int
calculateScoreToNorth (N{distanceFromParent = dist, turnL = tl, turnR = tr, direction = dir}) =
    dist + (1000 * (tl + tr + abs (turnsToDirection North dir)))

-- test' = putStrLn . drawTree . fmap (\n -> show (calculateScore n) ++ show n) $ buildPaths sp testInput
--   where
--     sp = findPoint testInput
-- test c dir = findBranches c dir (\_ v -> v == E) testInput

findMinimumScore :: [[Node Terrain]] -> Int
findMinimumScore = minimum . fmap getPathScore . findEndPaths
getPathScore = sum . fmap calculateScore
findEndPaths :: [[Node Terrain]] -> [[Node Terrain]]
findEndPaths = filter ((== E) . val . last)

solution1 :: ReindeerMap -> Int
solution1 ms =
    findMinimumScore paths
  where
    sp = findPoint S ms
    target = fst $ findPoint E ms
    paths = buildPaths sp target ms

december16Solution1 :: IO Int
december16Solution1 = solution1 <$> input

solution2 :: ReindeerMap -> Int
solution2 ms =
    (+ 1)
        . length
        . nub
        . concatMap (\xs -> pathToCoord xs ms detectEnd)
        . filter ((== minimumScore) . getPathScore)
        $ endPaths
  where
    sp = findPoint S ms
    target = fst $ findPoint E ms
    paths = buildPaths sp target ms
    minimumScore = findMinimumScore paths
    endPaths = findEndPaths paths

december16Solution2 :: IO Int
december16Solution2 = solution2 <$> input

testInput :: ReindeerMap
testInput =
    parseInput
        "###############\n\
        \#.......#....E#\n\
        \#.#.###.#.###.#\n\
        \#.....#.#...#.#\n\
        \#.###.#####.#.#\n\
        \#.#.#.......#.#\n\
        \#.#.#####.###.#\n\
        \#...........#.#\n\
        \###.#.#####.#.#\n\
        \#...#.....#.#.#\n\
        \#.#.#.###.#.#.#\n\
        \#.....#...#.#.#\n\
        \#.###.#.#.#.#.#\n\
        \#S..#.....#...#\n\
        \###############\n"

testInput' :: ReindeerMap
testInput' =
    parseInput
        "#################\n\
        \#...#...#...#..E#\n\
        \#.#.#.#.#.#.#.#.#\n\
        \#.#.#.#...#...#.#\n\
        \#.#.#.#.###.#.#.#\n\
        \#...#.#.#.....#.#\n\
        \#.#.#.#.#.#####.#\n\
        \#.#...#.#.#.....#\n\
        \#.#.#####.#.###.#\n\
        \#.#.#.......#...#\n\
        \#.#.###.#####.###\n\
        \#.#.#...#.....#.#\n\
        \#.#.#.#####.###.#\n\
        \#.#.#.........#.#\n\
        \#.#.#.#########.#\n\
        \#S#.............#\n\
        \#################\n"
