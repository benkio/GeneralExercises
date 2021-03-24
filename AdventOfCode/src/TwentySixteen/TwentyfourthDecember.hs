{-# LANGUAGE TupleSections #-}

module TwentySixteen.TwentyfourthDecember where

import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map (empty, filter, insert, toList, union, fromList, keys)
import Data.Set (Set, notMember)
import qualified Data.Set as Set (empty, fromList, union)

-- - Compute the path between all nodes: head to all the rest then tail until empty
--   Map ((Coordinate, Coordinate), Int)
-- - Look for salesman algorithm to solve the graph. permutations of all elements and find the min by looking at the graph

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

robotSearch :: Grid -> Set Coordinate -> [Coordinate] -> Coordinate -> Int -> IO Int
robotSearch grid visited positions targetPosition step
  | targetPosition `elem` positions = return step
  | otherwise = putStrLn ("visited: " ++ (show . length) visited) >> robotSearch grid (Set.union visited (Set.fromList positions)) nextPositions targetPosition (step + 1)
  where
    nextPositions = (filter (`notMember` visited) . concatMap neighboors) positions

buildGraph :: Grid -> [Coordinate] -> IO (Map (Coordinate, Coordinate) Int)
buildGraph _ [] = return Map.empty
buildGraph grid (h:hs) = do
  partialGraph <- mapM (\(c,c') -> (\x -> [((c,c'),x), ((c',c),x)]) <$> robotSearch grid Set.empty [c] c' 0) $ replicate (length hs) h `zip` hs
  Map.union ((Map.fromList . concat) partialGraph) <$> buildGraph grid hs

computeSteps :: Map (Coordinate, Coordinate) Int -> [Coordinate] -> Int
computeSteps graph hvacs = foldl (\acc x -> acc + (graph ! x)) 0 $ hvacs `zip` tail hvacs

search :: Grid -> IO Int
search grid = do
  let hvacs = (Map.keys . Map.filter isHVAC) grid
      initialRobotPosition = hvacPosition 0 grid
  graph <- buildGraph grid hvacs
  putStrLn $ show graph
  ((\(p, x) -> print p >> return x ) . minimumBy (\(_, a) (_, b) -> compare a b) . fmap (\p -> (p, computeSteps graph p)) . filter (\p -> head p == initialRobotPosition) . permutations) hvacs

inputTest :: Grid
inputTest =
  parseInput
    "###########\n\
    \#0.1.....2#\n\
    \#.#######.#\n\
    \#4.......3#\n\
    \###########"

test :: IO Bool
test = (== 14) <$> search inputTest

twentyfourthDecemberSolution1 :: IO Int
twentyfourthDecemberSolution1 = input >>= search

solution2 :: String -> Int
solution2 = undefined

twentyfourthDecemberSolution2 :: IO Int
twentyfourthDecemberSolution2 = undefined
