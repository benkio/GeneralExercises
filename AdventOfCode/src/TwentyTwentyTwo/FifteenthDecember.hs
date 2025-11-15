module TwentyTwentyTwo.FifteenthDecember where

import Data.Bifunctor (bimap)
import Data.Map (Map, fromList, lookup)
import Data.Maybe (mapMaybe)
import Debug.Trace
import Text.Printf
import Prelude hiding (lookup)

type Position = (Int, Int)

type SensorList = [(Position, Position)]

minMaxX :: [Position] -> Position
minMaxX [] = error "minMaxX on empty list"
minMaxX ss = (bimap minimum maximum . unzip) ss

minMaxXSensor :: (Position, Position) -> Position
minMaxXSensor (s@(sx, sy), b) =
    let md = manDis s b
     in (sx - md, sx + md)

input :: IO SensorList
input = parseInput <$> readFile "input/2022/15December.txt"

parseSensor :: String -> (Position, Position)
parseSensor s = ((parseSensorX s, parseSensorY s), (parseBeaconX s, parseBeaconY s))
  where
    stripTillEqual = tail . dropWhile (/= '=')
    parseSensorX = (\x -> read x :: Int) . takeWhile (/= ',') . stripTillEqual
    parseSensorY = (\x -> read x :: Int) . takeWhile (/= ':') . stripTillEqual . stripTillEqual
    parseBeaconX = (\x -> read x :: Int) . takeWhile (/= ',') . stripTillEqual . stripTillEqual . stripTillEqual
    parseBeaconY = (\x -> read x :: Int) . takeWhile (/= ':') . stripTillEqual . stripTillEqual . stripTillEqual . stripTillEqual

parseInput :: String -> SensorList
parseInput = fmap parseSensor . lines

testInput :: SensorList
testInput =
    parseInput
        "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\n\
        \Sensor at x=9, y=16: closest beacon is at x=10, y=16\n\
        \Sensor at x=13, y=2: closest beacon is at x=15, y=3\n\
        \Sensor at x=12, y=14: closest beacon is at x=10, y=16\n\
        \Sensor at x=10, y=20: closest beacon is at x=10, y=16\n\
        \Sensor at x=14, y=17: closest beacon is at x=10, y=16\n\
        \Sensor at x=8, y=7: closest beacon is at x=2, y=10\n\
        \Sensor at x=2, y=0: closest beacon is at x=2, y=10\n\
        \Sensor at x=0, y=11: closest beacon is at x=2, y=10\n\
        \Sensor at x=20, y=14: closest beacon is at x=25, y=17\n\
        \Sensor at x=17, y=20: closest beacon is at x=21, y=22\n\
        \Sensor at x=16, y=7: closest beacon is at x=15, y=3\n\
        \Sensor at x=14, y=3: closest beacon is at x=15, y=3\n\
        \Sensor at x=20, y=1: closest beacon is at x=15, y=3"

memberSensor :: Position -> SensorList -> Bool
memberSensor p = (\(ss, bs) -> p `elem` ss || p `elem` bs) . unzip

withinSensor :: Position -> SensorList -> Bool
withinSensor p [] = False
withinSensor p ((s, b) : ss)
    | manDis s b >= manDis s p = True
    | otherwise = withinSensor p ss

manDis :: Position -> Position -> Int
manDis (px, py) (px', py') = abs (px - px') + abs (py - py')

checkRow :: Int -> SensorList -> Int
checkRow y ss =
    let (minX, maxX) = (minMaxX . fmap minMaxXSensor) ss
     in (length . filter id . fmap (\x -> withinSensor (x, y) ss) . filter (\x -> not ((x, y) `memberSensor` ss))) [minX .. maxX]

fifteenthDecemberSolution1 :: IO Int
fifteenthDecemberSolution1 = checkRow 2000000 <$> input

sensorBounds :: Int -> (Position, Position) -> Maybe Position
sensorBounds y (s@(sx, sy), b)
    | y < sy - md || y > sy + md = Nothing
    | otherwise = case y `compare` sy of
        LT -> Just (minX + (sy - y), maxX - (sy - y))
        EQ -> Just (minX, maxX)
        GT -> Just (minX + (y - sy), maxX - (y - sy))
  where
    (minX, maxX) = minMaxXSensor (s, b)
    md = manDis s b

searchSignal :: Int -> SensorList -> Position
searchSignal bound ss =
    let vs = [0 .. bound]
     in findNonEmptyRow bound vs ss

findNonEmptyRow :: Int -> [Int] -> SensorList -> Position
findNonEmptyRow bound (y : ys) ssb
    | null vs = findNonEmptyRow bound ys ssb
    | otherwise = (head vs, y)
  where
    ssby = mapMaybe (sensorBounds y) ssb
    vs = filterRowSensors [(0, bound)] ssby

filterRowSensors :: [Position] -> [Position] -> [Int]
filterRowSensors xs [] = concatMap (\(x, x') -> [x .. x']) xs
filterRowSensors xs (p : ps) = filterRowSensors (filterRow xs p) ps

filterRow :: [Position] -> Position -> [Position]
filterRow [] _ = []
filterRow ((z, z') : xs) (xmin, xmax)
    | xmin >= z && xmax <= z' = (z, xmin - 1) : (xmax + 1, z') : filterRow xs (xmin, xmax) -- [  ( )  ]
    | z <= xmin && xmin <= z' = (z, xmin - 1) : filterRow xs (xmin, xmax) -- (  [ )  ]
    | xmin <= z && z <= xmax = (xmax + 1, z') : filterRow xs (xmin, xmax) -- [  ( ]  )
    | otherwise = (z, z') : filterRow xs (xmin, xmax)

solution2 :: Int -> SensorList -> Int
solution2 bound = (\(x, y) -> x * 4000000 + y) . searchSignal bound

fifteenthDecemberSolution2 :: IO Int
fifteenthDecemberSolution2 = solution2 4000000 <$> input
