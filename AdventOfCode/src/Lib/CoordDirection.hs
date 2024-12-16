module Lib.CoordDirection (changeDirection) where

import Lib.Direction (Direction(..), turnsToDirection)
import Lib.Coord (Coord)


{-
  Given a:
    - direction
    - currentPosition
    - targetPosition

  returns:
    - an int signaling how many turns to take (negative left, positive right)
    - new Direction 
-}
changeDirection :: Direction -> Coord -> Coord -> (Int,Direction)
changeDirection d c c' = (turnsToDirection d newDirection, newDirection)
  where
    newDirection = coordDirectionFromN c c'

coordDirectionFromN :: Coord -> Coord -> Direction
coordDirectionFromN (x,y) (a,b)
  | x == (a-1) && y == b = East
  | x == (a+1) && y == b = West
  | x == a && y == (b+1) = North
  | x == a && y == (b-1) = South
  | otherwise = error "The input coords are the same of at a distance > 1"
