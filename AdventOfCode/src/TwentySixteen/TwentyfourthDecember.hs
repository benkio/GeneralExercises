{-# LANGUAGE TupleSections #-}

module TwentySixteen.TwentyfourthDecember where

import Data.Map (Map, (!))
import qualified Data.Map as Map (adjust, empty, filter, insert, toList)
import Data.Set (Set, notMember)
import qualified Data.Set as Set (empty, fromList, union)

-- - Compute the path between all nodes: head to all the rest then tail until empty
--   Map ((Coordinate, Coordinate), Int)
-- - Look for salesman algorithm to solve the graph.

data Status = Open | Wall | HVAC Int deriving (Show, Eq)

type Coordinate = (Int, Int)

type Grid = Map Coordinate Status

input :: IO Grid
input = parseInput <$> readFile "input/2016/24December.txt"

parseInput :: String -> Grid
parseInput = Map.filter (not . isWall) . parseGrid . tail . init . lines

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

hvacId :: Status -> Maybe Int
hvacId (HVAC x) = Just x
hvacId _ = Nothing

isWall :: Status -> Bool
isWall Wall = True
isWall _ = False

isHVAC :: Status -> Bool
isHVAC (HVAC _) = True
isHVAC _ = False

maxCell :: Coordinate
maxCell = (176, 36)

neighboors :: Coordinate -> [Coordinate]
neighboors (x, y) = [(a, b) | a <- [max 0 (x -1) .. min (fst maxCell) (x + 1)], b <- [max 0 (y -1) .. min (snd maxCell) (y + 1)], (a == x || b == y) && (a /= x || b /= y)]

robotSearch :: Grid -> Set Coordinate -> [Coordinate] -> Int -> IO (Coordinate, Int)
robotSearch grid visited positions step
  | (not . null) newHVAC = return (head newHVAC, step)
  | otherwise = putStrLn ("visited: " ++ (show . length) visited) >> robotSearch grid (Set.union visited (Set.fromList positions)) nextPositions (step + 1)
  where
    newHVAC = filter (\c -> isHVAC (grid ! c)) positions
    nextPositions = (filter (`notMember` visited) . concatMap neighboors) positions

endCondition :: Grid -> Bool
endCondition = null . Map.filter isHVAC

search :: Grid -> Coordinate -> Int -> IO Int
search grid startingPoint acc
  | endCondition grid' = return acc
  | otherwise = do
    (newPosition, steps) <- robotSearch grid' Set.empty [startingPoint] 0
    putStrLn ("newPosition " ++ show newPosition ++ " steps " ++ show steps)
    search grid' newPosition (acc + steps)
  where
    grid' = Map.adjust (const Open) startingPoint grid

inputTest :: Grid
inputTest =
  parseInput
    "###########\n\
    \#0.1.....2#\n\
    \#.#######.#\n\
    \#4.......3#\n\
    \###########"

test :: IO Bool
test = (== 14) <$> search inputTest (0, 0) 0

twentyfourthDecemberSolution1 :: IO Int
twentyfourthDecemberSolution1 = input >>= (\i -> search i (startingPoint i) 0)
  where
    startingPoint = hvacPosition 0

solution2 :: String -> Int
solution2 = undefined

twentyfourthDecemberSolution2 :: IO Int
twentyfourthDecemberSolution2 = undefined
