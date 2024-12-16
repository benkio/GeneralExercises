module TwentyTwentyFour.December16 where

import Data.Bifunctor (bimap)
import Data.List (find, minimumBy)
import Data.Map (Map)
import Data.Map as Map (fromList, lookup, toList)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Tree (Tree, drawTree, foldTree)
import Data.Void
import Debug.Trace
import Lib.Coord (Coord, findCardinalNeighboors)
import Lib.CoordMap (findBranches)
import Lib.Direction (Direction (..))
import Lib.Parse (parseGridWithElemSelection)
import Lib.Pathfinding (Node (..), mapToTree)

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

buildTree :: (Coord, Terrain) -> ReindeerMap -> Tree (Node Terrain)
buildTree startingPoint = mapToTree startingPoint East (\_ v -> v == E)

findStartingPoint :: ReindeerMap -> (Coord, Terrain)
findStartingPoint = fromJust . find ((== S) . snd) . toList

-- foldTree :: (a -> [b] -> b) -> Tree a -> b
findBestPathScore :: Tree (Node Terrain) -> (Int, [Coord])
findBestPathScore = foldTree foldNCalculateScore

foldNCalculateScore :: Node Terrain -> [(Int, [Coord])] -> (Int, [Coord])
foldNCalculateScore n xs
    | val n == E = trace ("calculate on End " ++ show (calculateScore n)) (calculateScore n, [nc n])
    | null candidates = trace ("discard node " ++ show (nc n)) (-1, [])
    | otherwise = trace ("calculate on node " ++ show (nc n)) $ bimap (calculateScore n +) (nc n :) . minimumBy (comparing fst) $ candidates
  where
    candidates = filter ((>= 0) . fst) xs

calculateScore :: Node Terrain -> Int
calculateScore (N{distanceFromParent = dist, turnL = tl, turnR = tr}) = dist + (1000 * (tl + tr))

test' = putStrLn . drawTree . fmap (\n -> show (calculateScore n) ++ show n) $ buildTree sp testInput
  where
    sp = findStartingPoint testInput
test c dir = findBranches c dir (\_ v -> v == E) testInput

solution1 :: ReindeerMap -> Int
solution1 ms =
    fst $
        findBestPathScore tree
  where
    sp = findStartingPoint ms
    tree = buildTree sp ms

december16Solution1 :: IO Int
december16Solution1 = solution1 <$> input

solution2 :: ReindeerMap -> Int
solution2 = undefined

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
