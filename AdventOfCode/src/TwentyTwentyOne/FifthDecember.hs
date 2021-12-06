module TwentyTwentyOne.FifthDecember where

import Data.List (group, sort)

type Coord = (Int, Int)

type Line = (Coord, Coord)

input :: IO String
input = readFile "input/2021/5December.txt"

inputTest :: String
inputTest =
  "0,9 -> 5,9\n\
  \8,0 -> 0,8\n\
  \9,4 -> 3,4\n\
  \2,2 -> 2,1\n\
  \7,0 -> 7,4\n\
  \6,4 -> 2,0\n\
  \0,9 -> 2,9\n\
  \3,4 -> 1,4\n\
  \0,0 -> 8,8\n\
  \5,5 -> 8,2"

parseInput :: String -> [Line]
parseInput s = ((\l -> (head l, last l)) . fmap (\(x, y) -> ((read x :: Int), (read (tail y) :: Int))) . fmap (break (== ',')) . (\l -> [head l, last l]) . words) <$> lines s

expandLineCoords :: Coord -> Coord -> [Coord]
expandLineCoords (x, y) (x', y')
  | x == x' && y == y' = []
  | x == x' && y < y' = (x, y + 1) : expandLineCoords (x, y + 1) (x', y')
  | x == x' && y > y' = (x, y - 1) : expandLineCoords (x, y - 1) (x', y')
  | y == y' && x < x' = (x + 1, y) : expandLineCoords (x + 1, y) (x', y')
  | y == y' && x > x' = (x - 1, y) : expandLineCoords (x - 1, y) (x', y')
  -- TODO diagonals
  | otherwise = error "diagonals not implemented"

isHorizontal :: Line -> Bool
isHorizontal ((x, _), (x', _)) = x == x'

isVertical :: Line -> Bool
isVertical ((_, y), (_, y')) = y == y'

computeAllCoordinates :: [Line] -> [Coord]
computeAllCoordinates [] = []
computeAllCoordinates (l : ls)
  | isHorizontal l || isVertical l = (fst l : expandLineCoords (fst l) (snd l)) ++ computeAllCoordinates ls
  -- TODO: Add diagonals
  | otherwise = computeAllCoordinates ls

countOverlapCoordinates :: [Coord] -> Int
countOverlapCoordinates = length . filter ((> 1) . length) . group . sort

fifthDecemberSolution1 :: IO Int
fifthDecemberSolution1 = countOverlapCoordinates . computeAllCoordinates . parseInput <$> input

expandLineCoords' :: Coord -> Coord -> [Coord]
expandLineCoords' (x, y) (x', y')
  | x == x' && y == y' = []
  | x == x' && y < y' = (x, y + 1) : expandLineCoords' (x, y + 1) (x', y')
  | x == x' && y > y' = (x, y - 1) : expandLineCoords' (x, y - 1) (x', y')
  | y == y' && x < x' = (x + 1, y) : expandLineCoords' (x + 1, y) (x', y')
  | y == y' && x > x' = (x - 1, y) : expandLineCoords' (x - 1, y) (x', y')
  | x < x' && y < y' = (x + 1, y + 1) : expandLineCoords' (x + 1, y + 1) (x', y')
  | x < x' && y > y' = (x + 1, y - 1) : expandLineCoords' (x + 1, y - 1) (x', y')
  | x > x' && y > y' = (x - 1, y - 1) : expandLineCoords' (x - 1, y - 1) (x', y')
  | x > x' && y < y' = (x - 1, y + 1) : expandLineCoords' (x - 1, y + 1) (x', y')

computeAllCoordinates' :: [Line] -> [Coord]
computeAllCoordinates' [] = []
computeAllCoordinates' (l : ls) = (fst l : expandLineCoords' (fst l) (snd l)) ++ computeAllCoordinates' ls

fifthDecemberSolution2 :: IO Int
fifthDecemberSolution2 = countOverlapCoordinates . computeAllCoordinates' . parseInput <$> input
