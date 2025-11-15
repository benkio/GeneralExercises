{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module TwentyTwentyFour.December10 where

import Data.List (nub, partition)
import Data.Map (Map, fromList, toList, (!?))
import qualified Data.Map as Map (filter)
import Data.Maybe (mapMaybe)
import Data.Set (Set, insert)
import Debug.Trace

type Coord = (Int, Int)

type TrailMap = Map Coord Int

input :: IO TrailMap
input = parseInput <$> readFile "input/2024/December10.txt"

parseInput :: String -> TrailMap
parseInput =
    fromList
        . concatMap
            ( \(y, s) ->
                ( fmap (\(x, c) -> ((x, y), read [c] :: Int))
                    . zip [0 ..]
                )
                    s
            )
        . zip [0 ..]
        . lines

testInput :: TrailMap
testInput =
    parseInput
        "89010123\n\
        \78121874\n\
        \87430965\n\
        \96549874\n\
        \45678903\n\
        \32019012\n\
        \01329801\n\
        \10456732\n"

neighbours :: Coord -> [Coord]
neighbours (x, y) = [(a, b) | a <- [x - 1 .. x + 1], b <- [y - 1 .. y + 1], (a, b) /= (x, y), a == x || b == y]

trailStep :: TrailMap -> Coord -> Int -> [(Coord, Int)]
trailStep tr c v = if nextSlope > 9 then [] else ns
  where
    nextSlope = v + 1
    ns = filter ((== nextSlope) . snd) $ mapMaybe (\x -> (x,) <$> tr !? x) $ neighbours c

isPeak :: TrailMap -> Coord -> Bool
isPeak tr c = (Just 9 ==) $ tr !? c

trails :: TrailMap -> Coord -> Int -> [(Coord, Int)]
trails tr c v = peaks ++ concatMap (uncurry (trails tr)) noPeaks
  where
    nextSteps = trailStep tr c v
    (peaks, noPeaks) =
        partition (isPeak tr . fst) nextSteps

trailMapHeads :: TrailMap -> [(Coord, Int)]
trailMapHeads = toList . Map.filter (== 0)

solution1 :: TrailMap -> Int
solution1 tr = sum $ length . nub . uncurry (trails tr) <$> trailMapHeads tr

december10Solution1 :: IO Int
december10Solution1 = solution1 <$> input

trails' :: TrailMap -> [[(Coord, Int)]] -> [[(Coord, Int)]]
trails' tr = concatMap search
  where
    search [] = []
    search (reverse -> ((c, v) : trailRev)) =
        let trails = (\x -> reverse (x : (c, v) : trailRev)) <$> trailStep tr c v
            (endTrails, ongoingTrails) = span ((== 9) . snd . last) trails
         in endTrails ++ trails' tr ongoingTrails

testInput' :: TrailMap
testInput' =
    parseInput
        "8888808\n\
        \8843218\n\
        \8858828\n\
        \8865438\n\
        \8171848\n\
        \8887658\n\
        \8891888"

testInput'' =
    parseInput
        "3390339\n\
        \3331398\n\
        \8882117\n\
        \6543456\n\
        \7651987\n\
        \8761111\n\
        \9871111"

solution2 :: TrailMap -> Int
solution2 tr = length $ concatMap (\x -> trails' tr [[x]]) $ trailMapHeads tr

december10Solution2 :: IO Int
december10Solution2 = solution2 <$> input
