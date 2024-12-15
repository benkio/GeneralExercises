module Lib.CoordMove (coordMove) where

import Lib.Coord (Coord)
import Lib.Move (Move (..))

coordMove :: Move -> Coord -> Coord
coordMove U (x, y) = (x, y - 1)
coordMove D (x, y) = (x, y + 1)
coordMove L (x, y) = (x - 1, y)
coordMove R (x, y) = (x + 1, y)
