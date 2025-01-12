module Lib.Print (printGrid, printGridMap, printMove, printGridMapDefault) where

import Data.List (maximumBy)
import Data.Map (Map, findMax, keys, (!?))
import Data.Ord (comparing)
import Lib.Coord (Coord)
import Lib.Move (Move (..))

{-
  Print the coords in input by the function in input
  no empty spaces, just maximum boundaries
-}
printGrid :: Coord -> (Coord -> String) -> [Coord] -> String
printGrid (maxX, maxY) f cs = do
    y <- [0 .. maxY - 1]
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
printGridMap f ms = do
    y <- [0 .. maxY]
    x <- [0 .. maxX + 1]
    let c = if x == (maxX + 1) then "\n" else f (x, y) $ ms !? (x, y)
    c
  where
    maxX = fst . maximumBy (comparing fst) . keys $ ms
    maxY = snd . maximumBy (comparing snd) . keys $ ms

printGridMapDefault :: (Show a) => Map Coord a -> String
printGridMapDefault = printGridMap (\_ ma -> maybe "#" show ma)

printMove :: Move -> Char
printMove U = '^'
printMove D = 'v'
printMove L = '<'
printMove R = '>'
