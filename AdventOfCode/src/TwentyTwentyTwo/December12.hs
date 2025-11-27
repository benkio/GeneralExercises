module TwentyTwentyTwo.December12 where

import Data.Bifunctor (second)
import Data.List (find, minimumBy, sortOn, (\\))
import Data.Map (Map, elems, fromList, lookup)
import Data.Maybe (fromJust, mapMaybe)
import Debug.Trace
import Text.Printf
import Prelude hiding (lookup)

type Position = (Int, Int)

data Ground = Ground
    { position :: Position
    , height :: Char
    , distance :: Int
    , distanceTraveled :: Int
    }

instance Show Ground where
    show (Ground{position = p, height = h, distance = d, distanceTraveled = dt}) = printf "%s | %c | %d | %d" (show p) h d dt

instance Eq Ground where
    (==) g g' = position g == position g'
    (/=) g g' = position g /= position g'

input :: IO (Map Position Ground)
input = toGrid <$> readFile "input/2022/12December.txt"

toGrid :: String -> Map Position Ground
toGrid = fromList . concatMap (fmap amendStartEnd . uncurry parseRow) . (`zip` [0 ..]) . lines
  where
    parseRow row y = (fmap (\(c, x) -> Ground{position = (x, y), height = c, distance = maxBound :: Int, distanceTraveled = 0}) . (`zip` [0 ..])) row
    amendStartEnd g@(Ground{height = 'S'}) = (position g, g{height = '`'})
    amendStartEnd g@(Ground{height = 'E'}) = (position g, g{height = '{'})
    amendStartEnd g = (position g, g)

nextCandidate :: [Ground] -> Ground
nextCandidate = minimumBy (\g g' -> value g `compare` value g')
  where
    value = computeDistance'

getX :: Ground -> Int
getX = fst . position

getY :: Ground -> Int
getY = snd . position

getTarget :: Map Position Ground -> Ground
getTarget = fromJust . find ((== '{') . height) . elems

getStart :: Map Position Ground -> Ground
getStart = fromJust . find ((== '`') . height) . elems

computeDistance :: Ground -> Ground -> Int
computeDistance t x = abs x' + abs y'
  where
    x' = getX x - getX t
    y' = getY x - getY t

computeDistance' :: Ground -> Int
computeDistance' (Ground{distance = d, distanceTraveled = d', height = h}) = d + d' + (fromEnum '{' - fromEnum h) ^ 2

neighboors :: Map Position Ground -> Ground -> [Ground]
neighboors m g =
    filter (\x -> height x <= succ (height g)) $
        mapMaybe
            (`lookup` m)
            [(gx - 1, gy), (gx + 1, gy), (gx, gy + 1), (gx, gy - 1)]
  where
    gx = getX g
    gy = getY g

testInput :: Map Position Ground
testInput =
    toGrid
        "Sabqponm\n\
        \abcryxxl\n\
        \accszExk\n\
        \acctuvwj\n\
        \abdefghi"

findPath :: Map Position Ground -> [Ground] -> [Ground] -> Int
findPath _ [] _ = 0
findPath m open closed =
    let q = nextCandidate open
        open' = filter (/= q) open
        ns = neighboors m q
        target = getTarget m
        open'' = pushNeighboorsToOpen open' closed target q ns
     in if target `elem` ns && height q == 'z'
            then distanceTraveled q + 1
            else findPath m open'' (q : closed)

pushNeighboorsToOpen :: [Ground] -> [Ground] -> Ground -> Ground -> [Ground] -> [Ground]
pushNeighboorsToOpen open closed target q ns =
    foldl (\acc n -> if skippingCondition n then acc else n : filter ((/= position n) . position) acc) open ns'
  where
    ns' = fmap (\n -> n{distanceTraveled = distanceTraveled q + 1, distance = computeDistance target n}) ns
    skippingCondition n =
        any (\x -> x == n && computeDistance' x <= computeDistance' n) open
            || any (\x -> x == n && computeDistance' x <= computeDistance' n) closed

solution :: Ground -> Map Position Ground -> Int
solution start m =
    let open = [start]
     in findPath m open []

december12Solution1 :: IO Int
december12Solution1 = (\m -> solution (getStart m) m) <$> input

december12Solution2 :: IO Int
december12Solution2 = solution2 <$> input

solution2 :: Map Position Ground -> Int
solution2 m =
    let as = (filter (\g -> (height g == 'a' || height g == '`') && quickToC m g) . elems) m
     in minimum $ fmap (`solution` m) as

quickToC :: Map Position Ground -> Ground -> Bool
quickToC m h = any (any ((== 'c') . height) . neighboors m) (neighboors m h)
