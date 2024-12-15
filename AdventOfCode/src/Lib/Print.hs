module Lib.Print (printGrid) where

import Lib.Coord (Coord)

{-
  Print the coords in input by the function in input
  no empty spaces, just maximum boundaries
-}
printGrid :: Coord -> (Coord -> Char) -> [Coord] -> String
printGrid (maxX, maxY) f cs = do
    y <- [0 .. maxY -1]
    x <- [0 .. maxX]
    let c = case (x, y) of
            c | c `elem` cs -> f (x, y)
            c | x == maxX -> '\n'
            c -> '.'
    return c
