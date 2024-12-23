module Lib.CoordMove (coordMove, manhattanDistanceSignedToMove) where

import Lib.Coord (Coord)
import Lib.Move (Move (..))

coordMove :: Move -> Coord -> Coord
coordMove U (x, y) = (x, y - 1)
coordMove D (x, y) = (x, y + 1)
coordMove L (x, y) = (x - 1, y)
coordMove R (x, y) = (x + 1, y)

manhattanDistanceSignedToMove :: Coord -> [Move]
manhattanDistanceSignedToMove (x,y) = horizontalMoves ++ verticalMoves
  where
    horizontalMoves = if x < 0 then replicate (abs x) L else replicate x R
    verticalMoves = if y < 0 then replicate (abs y) U else replicate y D
