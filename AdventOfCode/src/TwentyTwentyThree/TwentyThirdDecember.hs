{-# LANGUAGE LambdaCase #-}

module TwentyTwentyThree.TwentyThirdDecember where

import Data.Map (Map, findMax)
import qualified Data.Map as M (fromList, lookup)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as S (empty, insert, notMember, size)

data FieldBlock = Empty | SlopeU | SlopeL | SlopeD | SlopeR deriving (Eq)
type HikeMap = Map (Int, Int) FieldBlock
data Direction = U | D | L | R deriving (Show, Enum, Eq)
data Path = Path
    { visited :: Set (Int, Int)
    , current :: (Int, Int)
    , dir :: Direction
    }
    deriving (Show)

instance Show FieldBlock where
    show Empty = "."
    show SlopeU = "^"
    show SlopeR = ">"
    show SlopeD = "v"
    show SlopeL = "<"

instance Read FieldBlock where
    readsPrec _ = \case
        '.' : xs -> [(Empty, xs)]
        '^' : xs -> [(SlopeU, xs)]
        '>' : xs -> [(SlopeR, xs)]
        'v' : xs -> [(SlopeD, xs)]
        '<' : xs -> [(SlopeL, xs)]
        xs -> []

input :: IO HikeMap
input = parseInput <$> readFile "input/2023/23December.txt"

parseInput :: String -> HikeMap
parseInput = M.fromList . concat . zipWith parseLine [0 ..] . lines
  where
    parseLine :: Int -> String -> [((Int, Int), FieldBlock)]
    parseLine y = catMaybes . zipWith (\x c -> if c /= '#' then Just ((x, y), read [c]) else Nothing) [0 ..]

startingPath :: Path
startingPath = Path{visited = S.empty, current = (1, 0), dir = D}

endingPoint :: HikeMap -> (Int, Int)
endingPoint = fst . findMax

nextDirections :: Direction -> [Direction]
nextDirections d = filter (/= (reverseDirection d)) $ enumFrom U
reverseDirection :: Direction -> Direction
reverseDirection U = D
reverseDirection R = L
reverseDirection D = U
reverseDirection L = R
step :: (Int, Int) -> Direction -> (Int, Int)
step (x, y) U = (x, y - 1)
step (x, y) D = (x, y + 1)
step (x, y) L = (x - 1, y)
step (x, y) R = (x + 1, y)
isNotForest :: (Int, Int) -> HikeMap -> Bool
isNotForest p hm = isJust $ M.lookup p hm
isNotCounterSlope :: (Int, Int) -> HikeMap -> Direction -> Bool
isNotCounterSlope p hm U = fromMaybe False $ (/= SlopeD) <$> M.lookup p hm
isNotCounterSlope p hm D = fromMaybe False $ (/= SlopeU) <$> M.lookup p hm
isNotCounterSlope p hm L = fromMaybe False $ (/= SlopeR) <$> M.lookup p hm
isNotCounterSlope p hm R = fromMaybe False $ (/= SlopeL) <$> M.lookup p hm

hikeStep :: Path -> HikeMap -> [((Int, Int), Direction)]
hikeStep (Path{visited = path, current = p, dir = dir}) hm = filter ((`S.notMember` path) . fst) nextStepsNonForest
  where
    ds = nextDirections dir
    nextStepsCoordDirection = fmap (\d -> (step p d, d)) ds
    nextStepsNonForest = filter (\(p', d) -> isNotForest p' hm && isNotCounterSlope p' hm d) nextStepsCoordDirection

walkPath :: Path -> HikeMap -> [Path]
walkPath path hm =
    fmap (updatePath path) pathNextSteps
  where
    pathNextSteps = hikeStep path hm
    updatePath :: Path -> ((Int, Int), Direction) -> Path
    updatePath (Path{visited = vs, current = p, dir = dir}) (p', d') =
        (Path{visited = S.insert p vs, current = p', dir = d'})

walkPaths :: [Path] -> HikeMap -> [Path]
walkPaths [] hm = []
walkPaths (p : ps) hm = terminatingPaths ++ walkPaths (ps ++ walkPath p hm) hm
  where
    nextPaths = walkPath p hm
    terminatingPaths = filter (\x -> current x == endingPoint hm) nextPaths

pathLength :: Path -> Int
pathLength = S.size . visited

testInput :: HikeMap
testInput =
    parseInput
        "#.#####################\n\
        \#.......#########...###\n\
        \#######.#########.#.###\n\
        \###.....#.>.>.###.#.###\n\
        \###v#####.#v#.###.#.###\n\
        \###.>...#.#.#.....#...#\n\
        \###v###.#.#.#########.#\n\
        \###...#.#.#.......#...#\n\
        \#####.#.#.#######.#.###\n\
        \#.....#.#.#.......#...#\n\
        \#.#####.#.#.#########v#\n\
        \#.#...#...#...###...>.#\n\
        \#.#.#v#######v###.###v#\n\
        \#...#.>.#...>.>.#.###.#\n\
        \#####v#.#.###v#.#.###.#\n\
        \#.....#...#...#.#.#...#\n\
        \#.#########.###.#.#.###\n\
        \#...###...#...#...#.###\n\
        \###.###.#.###v#####v###\n\
        \#...#...#.#.>.>.#.>.###\n\
        \#.###.###.#.###.#.#v###\n\
        \#.....###...###...#...#\n\
        \#####################.#"

solution1 = maximum . fmap pathLength . walkPaths [startingPath]

twentythirdDecemberSolution1 :: IO Int
twentythirdDecemberSolution1 = solution1 <$> input

solution2 = undefined

twentythirdDecemberSolution2 :: IO Int
twentythirdDecemberSolution2 = undefined
