module Lib.Coord (
    Coord,
    cardinalNeighboors,
    ordinalNeighboors,
    coordDistance,
    onTheSameLine,
    isCardinalNeighboor,
    isOrdinalNeighboor,
) where

import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.Map (Map, toList, (!?))
import qualified Data.Map as M (filterWithKey)
import Data.Maybe (listToMaybe, mapMaybe)

type Coord = (Int, Int)

cardinalNeighboors, ordinalNeighboors :: Coord -> [Coord]
cardinalNeighboors (x, y) = [(a, b) | a <- [x - 1 .. x + 1], b <- [y - 1 .. y + 1], a == x || b == y, (a, b) /= (x, y)]
ordinalNeighboors (x, y) = [(a, b) | a <- [x - 1 .. x + 1], b <- [y - 1 .. y + 1], (a, b) /= (x, y)]

isCardinalNeighboor, isOrdinalNeighboor :: Coord -> Coord -> Bool
isCardinalNeighboor (x, y) c = c `elem` cardinalNeighboors (x, y)
isOrdinalNeighboor (x, y) c = c `elem` ordinalNeighboors (x, y)

coordDistance :: Coord -> Coord -> (Int, Int)
coordDistance (x, y) (a, b) = (abs (x - a), abs (y - b))

onTheSameLine :: Coord -> Coord -> Bool
onTheSameLine (x, y) (a, b)
    | x == a || y == b = True
    | otherwise = False
