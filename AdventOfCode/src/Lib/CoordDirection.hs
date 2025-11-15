module Lib.CoordDirection (
    changeDirection,
    coordDirection,
)
where

import Data.Functor ((<&>))
import Data.Map (Map, toList, (!?))
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Debug.Trace
import Lib.Coord (Coord)
import Lib.Direction (Direction (..), turn180, turnsToDirection)
import Text.Printf (printf)

{-
  Given a:
    - direction
    - currentPosition
    - targetPosition

  returns:
    - an int signaling how many turns to take (negative left, positive right)
    - new Direction
-}
changeDirection :: Direction -> Coord -> Coord -> (Int, Direction)
changeDirection d c c' = (turnsToDirection d newDirection, newDirection)
  where
    newDirection = coordDirectionFromN c c'

coordDirectionFromN :: Coord -> Coord -> Direction
coordDirectionFromN (x, y) (a, b)
    | x == (a - 1) && y == b = East
    | x == (a + 1) && y == b = West
    | x == a && y == (b + 1) = North
    | x == a && y == (b - 1) = South
    | otherwise = error "The input coords are the same of at a distance > 1"

coordDirection :: Direction -> Coord -> Coord
coordDirection North (x, y) = (x, y - 1)
coordDirection South (x, y) = (x, y + 1)
coordDirection West (x, y) = (x - 1, y)
coordDirection East (x, y) = (x + 1, y)
