module Lib.Coord (
    cardinalNeighboors,
    coordPlus,
    inside,
    insideRect,
    isCardinalNeighboor,
    isOrdinalNeighboor,
    manhattanDistance',
    manhattanDistance,
    manhattanDistanceSigned,
    manhattanPath,
    onTheSameLine,
    ordinalNeighboors,
    subtractRect,
    coordsRect,
    hasIntersectionRect,
    Coord,
)
where

import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.Map (Map, toList, (!?))
import qualified Data.Map as M (filterWithKey)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe, isNothing)

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

inside :: Coord -> Coord -> Coord -> Bool
inside (x, y) (sx, sy) (ex, ey) =
    x >= sx
        && x <= ex
        && y >= sy
        && y <= ey

{- | Normalize a rectangle to ensure (topLeft, bottomRight) format
where topLeft = (minX, minY) and bottomRight = (maxX, maxY)
-}
normalizeRect :: (Coord, Coord) -> (Coord, Coord)
normalizeRect ((x1, y1), (x2, y2)) = ((min x1 x2, min y1 y2), (max x1 x2, max y1 y2))

-- | Check if two rectangles intersect
hasIntersectionRect :: (Coord, Coord) -> (Coord, Coord) -> Maybe (Coord, Coord)
hasIntersectionRect ((minX1, minY1), (maxX1, maxY1)) ((minX2, minY2), (maxX2, maxY2))
    | minX2 <= maxX1 && maxX2 >= minX1 && minY2 <= maxY1 && maxY2 >= minY1 =
        Just ((max minX1 minX2, max minY1 minY2), (min maxX1 maxX2, min maxY1 maxY2))
    | otherwise = Nothing

-- | Check if the second rectangle is completely inside the first
insideRect :: (Coord, Coord) -> (Coord, Coord) -> Bool
insideRect ((minX1, minY1), (maxX1, maxY1)) ((minX2, minY2), (maxX2, maxY2)) =
    minX2 >= minX1
        && maxX2 <= maxX1
        && minY2 >= minY1
        && maxY2 <= maxY1

{- | Given two pairs of coords representing square blocks (topLeft and bottomRight corners),
returns a list of square blocks consisting of the area in the second block
that is not in the first block.
If there's no intersection, returns the second block.
If the second block is totally included in the first, returns an empty list.
All output rectangles are guaranteed to be in (topLeft, bottomRight) format.
-}
subtractRect :: (Coord, Coord) -> (Coord, Coord) -> [(Coord, Coord)]
subtractRect rect1@((minX1, minY1), (maxX1, maxY1)) rect2@((minX2, minY2), (maxX2, maxY2))
    | isNothing (hasIntersectionRect rect1 rect2) = [normalizeRect rect2]
    | otherwise =
        -- Split the second rectangle into parts that don't overlap with first
        -- All rectangles are constructed as (topLeft, bottomRight) and normalized
        map normalizeRect . catMaybes $
            [ -- Top rectangle (above the intersection)
              if minY2 < minY1
                then Just ((minX2, minY2), (maxX2, minY1 - 1))
                else Nothing
            , -- Bottom rectangle (below the intersection)
              if maxY2 > maxY1
                then Just ((minX2, maxY1 + 1), (maxX2, maxY2))
                else Nothing
            , -- Left rectangle (left of the intersection)
              if minX2 < minX1
                then Just ((minX2, max minY2 minY1), (minX1 - 1, min maxY2 maxY1))
                else Nothing
            , -- Right rectangle (right of the intersection)
              if maxX2 > maxX1
                then Just ((maxX1 + 1, max minY2 minY1), (maxX2, min maxY2 maxY1))
                else Nothing
            ]

coordsRect :: (Coord, Coord) -> Int
coordsRect ((minX, minY), (maxX, maxY)) =
    (maxX - minX + 1) * (maxY - minY + 1)
