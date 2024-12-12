module Lib.Coord (Coord, findCardinalNeighboors, findOrdinalNeighboors) where

import Data.Map (Map)
import qualified Data.Map as M (filterWithKey)

type Coord = (Int, Int)

findCardinalNeighboors, findOrdinalNeighboors :: Coord -> Map Coord a -> Map Coord a
findCardinalNeighboors c = M.filterWithKey (\k _ -> isCardinalNeighboor c k)
findOrdinalNeighboors c = M.filterWithKey (\k _ -> isOrdinalNeighboor c k)

isCardinalNeighboor, isOrdinalNeighboor :: Coord -> Coord -> Bool
isCardinalNeighboor (x, y) c = c `elem` [(a, b) | a <- [x - 1 .. x + 1], b <- [y - 1 .. y + 1], a == x || b == y, (a, b) /= (x, y)]
isOrdinalNeighboor (x, y) c = c `elem` [(a, b) | a <- [x - 1 .. x + 1], b <- [y - 1 .. y + 1], (a, b) /= (x, y)]
