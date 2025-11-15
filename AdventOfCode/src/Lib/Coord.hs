module Lib.Coord (
    Coord,
    cardinalNeighboors,
    ordinalNeighboors,
    manhattanDistance,
    manhattanDistanceSigned,
    manhattanDistance',
    manhattanPath,
    onTheSameLine,
    isCardinalNeighboor,
    isOrdinalNeighboor,
    coordPlus,
)
where

import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.Map (Map, toList, (!?))
import qualified Data.Map as M (filterWithKey)
import Data.Maybe (listToMaybe, mapMaybe)

type Coord = (Int, Int)

coordPlus :: Coord -> Coord -> Coord
coordPlus (x, y) (a, b) = (x + a, y + b)

cardinalNeighboors, ordinalNeighboors :: Coord -> [Coord]
cardinalNeighboors (x, y) = [(a, b) | a <- [x - 1 .. x + 1], b <- [y - 1 .. y + 1], a == x || b == y, (a, b) /= (x, y)]
ordinalNeighboors (x, y) = [(a, b) | a <- [x - 1 .. x + 1], b <- [y - 1 .. y + 1], (a, b) /= (x, y)]

isCardinalNeighboor, isOrdinalNeighboor :: Coord -> Coord -> Bool
isCardinalNeighboor (x, y) c = c `elem` cardinalNeighboors (x, y)
isOrdinalNeighboor (x, y) c = c `elem` ordinalNeighboors (x, y)

manhattanDistance :: Coord -> Coord -> (Int, Int)
manhattanDistance (x, y) (a, b) = (abs (x - a), abs (y - b))

manhattanDistanceSigned :: Coord -> Coord -> (Int, Int)
manhattanDistanceSigned (x, y) (a, b) = (x - a, y - b)

manhattanDistance' :: Coord -> Coord -> Int
manhattanDistance' c c' = uncurry (+) $ manhattanDistance c c'

manhattanPath :: Coord -> Coord -> [Coord]
manhattanPath (x, y) (a, b)
    | x <= a && y <= b = [(x', y') | x' <- [x .. a], y' <- [y .. b], y' == y || x' == a]
    | x <= a && y > b = [(x', y') | x' <- [x .. a], y' <- reverse [b .. y], y' == y || x' == a]
    | x > a && y <= b = reverse [(x', y') | x' <- [a .. x], y' <- reverse [y .. b], y' == y || x' == a]
    | x > a && y > b = reverse [(x', y') | x' <- [a .. x], y' <- [b .. y], y' == b || x' == x]

onTheSameLine :: Coord -> Coord -> Bool
onTheSameLine (x, y) (a, b)
    | x == a || y == b = True
    | otherwise = False
