module Lib.Print (printGrid, printGridMap, printMove) where

import Lib.Coord (Coord)
import Lib.Move (Move(..))
import Data.Map (Map, findMax ,(!?))

{-
  Print the coords in input by the function in input
  no empty spaces, just maximum boundaries
-}
printGrid :: Coord -> (Coord -> String) -> [Coord] -> String
printGrid (maxX, maxY) f cs = do
    y <- [0 .. maxY -1]
    x <- [0 .. maxX]
    let c = case (x, y) of
            c | c `elem` cs -> f (x, y)
            c | x == maxX -> "\n"
            c -> "."
    c

{-
  Print the coords in input by the function in input
  no empty spaces, just maximum boundaries
-}
printGridMap :: (Coord -> Maybe a -> String) -> Map Coord a -> String
printGridMap  f ms = do
    y <- [0 .. maxY]
    x <- [0 .. maxX + 1]
    let c = if x == (maxX +1) then "\n" else  f (x, y) $ ms !? (x, y)
    c
  where
    ((maxX, maxY),_) = findMax ms

printMove :: Move -> Char
printMove U = '^'
printMove D = 'v'
printMove L = '<'
printMove R = '>'
