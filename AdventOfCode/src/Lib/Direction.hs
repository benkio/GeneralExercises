module Lib.Direction (
  Direction(..),
    turn90,
    turn180,
    turn270,
    turn90N,
    turnsToDirection
    ) where

import Data.Bifunctor

data Direction = North | South | East | West deriving (Show, Eq)

turn90, turn180, turn270 :: Direction -> Direction
turn90 North = East
turn90 East = South
turn90 South = West
turn90 West = North
turn180 = turn90N 2
turn270 = turn90N 3

turn90N :: Int -> Direction -> Direction
turn90N i= (!! i) . iterate turn90

turnsToDirection :: Direction -> Direction -> Int
turnsToDirection d d' = (\i -> if i == 3 then (-1) else i) . snd $ until ((==d') . fst) (bimap turn90 (+1)) (d,0)

