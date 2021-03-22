{-# LANGUAGE TupleSections #-}

module TwentySixteen.TwentyfourthDecember where

import Data.Map (Map, (!))
import qualified Data.Map as Map (elems, empty, filter, fromList, insert, keys, lookup, toList, union)

-- the idea is to have a:
--  - Map Coordinate Status indicate what is in the map
--  - The initial position (Coordinate, Int) that is the position of the robot.
--  - A function taking the list of Current Coordinates & the visited
--    coordinates. It returns the available next coordinates that are not
--    into the visited and the updated visited, adding the starting
--    points to the visited.
--  - Run the previous function with the initial position until a number
--    is found. At each call increase a number that is the number of
--    steps.
--  - use the previous function again to find the first element to visit,
--    collect it, update the position found with an empty space. Redo
--    until all elements are reached.
--  - sum up the steps to reach each elements. profit

data Status = Open | Wall | HVAC Int deriving (Show, Eq)

type Coordinate = (Int, Int)

type Grid = Map Coordinate Status

input :: IO Grid
input = Map.filter (not . isWall) . parseGrid . tail . init . lines <$> readFile "input/2016/24December.txt"

parseGrid :: [String] -> Grid
parseGrid = fst . foldl foldLine (Map.empty, 0)
  where
    foldLine :: (Grid, Int) -> String -> (Grid, Int)
    foldLine (grid, y) =
      (,y + 1) . fst
        . foldl
          ( \(g, x) c -> case c of
              '.' -> (Map.insert (x, y) Open g, x + 1)
              '#' -> (Map.insert (x, y) Wall g, x + 1)
              _ -> (Map.insert (x, y) (HVAC (read [c] :: Int)) g, x + 1)
          )
          (grid, 0)
        . tail
        . init

hvacPosition :: Int -> Grid -> Coordinate
hvacPosition hvacId = fst . head . Map.toList . Map.filter (HVAC hvacId ==)

isWall :: Status -> Bool
isWall Wall = True
isWall _ = False

isHVAC :: Status -> Bool
isHVAC (HVAC _) = True
isHVAC _ = False

maxCell :: Coordinate
maxCell = (176, 36)

neighboors :: Coordinate -> [Coordinate]
neighboors (x, y) = [(a, b) | a <- [max 0 (x -1) .. min (fst maxCell) (x + 1)], b <- [max 0 (y -1) .. min (snd maxCell) (y + 1)], (a == x || b == y) && (a /= x && b /= y)]

robotSearch :: Grid -> Grid -> Grid -> Int -> (Status, Int)
robotSearch grid visited positions step
  | (not . null) newHVAC = (head newHVAC, step)
  | otherwise = robotSearch grid (Map.union visited positions) nextPositions (step + 1)
  where
    visitedHVAC = (filter isHVAC . Map.elems) visited
    newHVAC = (filter (\c -> isHVAC c && c `notElem` visitedHVAC) . Map.elems) positions
    nextPositions = (Map.fromList . fmap (\c -> (c, grid ! c)) . filter (`notElem` Map.keys visited) . concatMap neighboors . Map.keys) positions

solution1 :: String -> Int
solution1 = undefined

twentyfourthDecemberSolution1 :: IO Int
twentyfourthDecemberSolution1 = undefined

solution2 :: String -> Int
solution2 = undefined

twentyfourthDecemberSolution2 :: IO Int
twentyfourthDecemberSolution2 = undefined
