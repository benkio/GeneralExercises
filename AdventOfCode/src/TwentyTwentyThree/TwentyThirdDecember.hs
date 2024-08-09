{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module TwentyTwentyThree.TwentyThirdDecember where

import Text.Printf (printf)

import Data.List (group, partition, sort)
import Data.Map (Map, findMax)
import qualified Data.Map as M (fromList, lookup)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as S (empty, filter, fromList, insert, notMember, size, toList)
import Debug.Trace

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
startingPath = Path{visited = S.empty, current = startingPoint, dir = D}
startingPoint :: (Int, Int)
startingPoint = (1, 0)
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

hikeStep :: Set (Int, Int) -> (Int, Int) -> Direction -> HikeMap -> Bool -> [((Int, Int), Direction)]
hikeStep path p dir hm slopeCheck = filter ((`S.notMember` path) . fst) nextStepsNonForest
  where
    ds = nextDirections dir
    nextStepsCoordDirection = fmap (\d -> (step p d, d)) ds
    nextStepsNonForest = filter (\(p', d) -> isNotForest p' hm && (not slopeCheck || isNotCounterSlope p' hm d)) nextStepsCoordDirection

walkPath :: Path -> HikeMap -> [Path]
walkPath path hm =
    fmap (updatePath path) pathNextSteps
  where
    pathNextSteps = hikeStep (visited path) (current path) (dir path) hm True
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

data GraphPath = GraphPath
    { gpLength :: Int
    , gpStart :: (Int, Int)
    , gpEnd :: (Int, Int)
    }
    deriving (Eq, Ord, Show)

buildGraphPathsSingle :: (Int, Int) -> Direction -> HikeMap -> ([GraphPath], [((Int, Int), Direction)])
buildGraphPathsSingle start d hm = go (step start d) d 1
  where
    go current dir stepCount
        | length steps == 1 = go ((fst . head) steps) ((snd . head) steps) (stepCount + 1)
        | otherwise =
            (
                [ GraphPath{gpLength = stepCount, gpStart = start, gpEnd = current}
                , GraphPath{gpLength = stepCount, gpStart = current, gpEnd = start}
                ]
            , fmap ((current,) . snd) steps
            )
      where
        steps = hikeStep S.empty current dir hm False

buildGraphPath :: (Int, Int) -> Direction -> HikeMap -> Set GraphPath
buildGraphPath start d hm = go [(start, d)] S.empty
  where
    go [] gs = gs
    go ((current, dir) : cs) gs =
        let
            (gs', next) = buildGraphPathsSingle current dir hm
            newgs = filter (`notElem` gs) gs'
         in
            if null newgs
                then go cs gs
                else go (cs ++ next) (foldl (\s x -> S.insert x s) gs newgs)

-- given a node terminal return the adjacent paths
findAdjacentPaths :: [GraphPath] -> (Int, Int) -> Set GraphPath -> [GraphPath]
findAdjacentPaths path node = S.toList . S.filter (\gp -> gpStart gp == node && gpEnd gp `notElem` concatMap (\x -> [gpEnd x, gpStart x]) path)

-- given a list of graphPath it returns its step size
graphPathSize :: [GraphPath] -> Int
graphPathSize = sum . fmap gpLength

-- given the list of all the graph path, the current path and current position
-- returns a list of all the grap path that can be added from that one. filtering by duplicates
buildPathsSingle :: [GraphPath] -> (Int, Int) -> Set GraphPath -> [[GraphPath]]
buildPathsSingle currentGP currentPos allPaths = newPaths
  where
    adjacentPaths = findAdjacentPaths currentGP currentPos allPaths
    newPaths = fmap ((currentGP ++) . (: [])) adjacentPaths

-- given the paths that are being built, keep building till all the paths ending to the end node are generated.
buildPath :: HikeMap -> Set GraphPath -> [[GraphPath]] -> Int -> Int
buildPath _ _ [] v = v
buildPath hm allGp (gp : gps) !v =
    buildPath hm allGp gps' v'
  where
    end = endingPoint hm
    currentPoint = if null gp then startingPoint else (gpEnd . last) gp
    newGps = buildPathsSingle gp currentPoint allGp
    (endingGps, nextGps) = partition ((== end) . gpEnd . last) newGps
    gps' = S.toList $ S.fromList (gps ++ nextGps)
    v' = (\x -> trace (printf "debug: %s - %d" (show x) (length gps')) x) $ foldl (\m gp -> max m (graphPathSize gp)) v endingGps

solution2 hm =
    -- graphPathSize . maximumBy (\p p' -> graphPathSize p `compare` graphPathSize p')
    buildPath hm allPaths [firstPath] 0
  where
    allPaths = buildGraphPath (1, 0) D hm
    firstPath = S.toList $ S.filter ((== (1, 0)) . gpStart) allPaths

-- 6238 is too low
-- twentythirdDecemberSolution2 :: IO Int
twentythirdDecemberSolution2 = solution2 <$> input
