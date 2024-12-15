module Lib.Coord (
    Coord,
    findCardinalNeighboors,
    findOrdinalNeighboors,
    cardinalNeighboors,
    ordinalNeighboors,
    coordDistance
) where

import Data.Map (Map)
import qualified Data.Map as M (filterWithKey)

type Coord = (Int, Int)

findCardinalNeighboors, findOrdinalNeighboors :: Coord -> Map Coord a -> Map Coord a
findCardinalNeighboors c = M.filterWithKey (\k _ -> isCardinalNeighboor c k)
findOrdinalNeighboors c = M.filterWithKey (\k _ -> isOrdinalNeighboor c k)

cardinalNeighboors, ordinalNeighboors :: Coord -> [Coord]
cardinalNeighboors (x, y) = [(a, b) | a <- [x - 1 .. x + 1], b <- [y - 1 .. y + 1], a == x || b == y, (a, b) /= (x, y)]
ordinalNeighboors (x, y) = [(a, b) | a <- [x - 1 .. x + 1], b <- [y - 1 .. y + 1], (a, b) /= (x, y)]

isCardinalNeighboor, isOrdinalNeighboor :: Coord -> Coord -> Bool
isCardinalNeighboor (x, y) c = c `elem` cardinalNeighboors (x, y)
isOrdinalNeighboor (x, y) c = c `elem` ordinalNeighboors (x, y)

coordDistance :: Coord -> Coord -> (Int, Int)
coordDistance (x,y) (a,b) = (abs (x-a), abs (y-b))
