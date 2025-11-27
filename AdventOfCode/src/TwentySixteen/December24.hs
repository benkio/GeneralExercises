{-# LANGUAGE TupleSections #-}

module TwentySixteen.December24 where

import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map (empty, filter, findMax, fromList, insert, keys, toList, union)
import Data.Set (Set, notMember)
import qualified Data.Set as Set (empty, fromList, union)

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
        (,y + 1)
            . fst
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
hvacPosition hId = fst . head . Map.toList . Map.filter (HVAC hId ==)

hvacId :: Status -> Maybe Int
hvacId (HVAC x) = Just x
hvacId _ = Nothing

isWall :: Status -> Bool
isWall Wall = True
isWall _ = False

isHVAC :: Status -> Bool
isHVAC (HVAC _) = True
isHVAC _ = False

neighboors :: Coordinate -> Coordinate -> [Coordinate]
neighboors maxCell (x, y) = [(a, b) | a <- [max 0 (x - 1) .. min (fst maxCell) (x + 1)], b <- [max 0 (y - 1) .. min (snd maxCell) (y + 1)], (a == x || b == y) && (a /= x || b /= y)]

robotSearch :: Grid -> Set Coordinate -> [Coordinate] -> [Coordinate] -> Int -> [(Coordinate, Int)]
robotSearch _ _ _ [] _ = []
robotSearch grid visited positions targetPositions step
    | any (`elem` targetPositions) positions =
        let hvac = filter (`elem` targetPositions) positions
         in fmap (,step) hvac ++ robotSearch grid visited positions (targetPositions \\ hvac) step
    | otherwise = robotSearch grid (Set.union visited (Set.fromList positions)) nextPositions targetPositions (step + 1)
  where
    nextPositions = (filter (\x -> x `notMember` visited && x `elem` Map.keys grid) . nub . concatMap (neighboors ((fst . Map.findMax) grid))) positions

buildGraph :: Grid -> [Coordinate] -> Map (Coordinate, Coordinate) Int
buildGraph _ [] = Map.empty
buildGraph grid (h : hs) =
    let partialGraph = (\(c, x) -> [((h, c), x), ((c, h), x)]) <$> robotSearch grid Set.empty [h] hs 0
     in Map.union ((Map.fromList . concat) partialGraph) $ buildGraph grid hs

computeSteps :: Map (Coordinate, Coordinate) Int -> [Coordinate] -> Int
computeSteps graph hvacs = foldl (\acc x -> acc + (graph ! x)) 0 $ hvacs `zip` tail hvacs

search :: [Coordinate] -> ([Coordinate] -> Bool) -> Grid -> Int
search hvacs filterPermutations grid =
    (minimum . fmap (computeSteps (buildGraph grid hvacs)) . filter filterPermutations . permutations) hvacs

inputTest :: Grid
inputTest =
    parseInput
        "###########\n\
        \#0.1.....2#\n\
        \#.#######.#\n\
        \#4.......3#\n\
        \###########"

test :: Bool
test = search hvacs filterPermutations inputTest == 14
  where
    hvacs = (Map.keys . Map.filter isHVAC) inputTest
    initialRobotPosition = hvacPosition 0 inputTest
    filterPermutations p = head p == initialRobotPosition

december24Solution1 :: IO Int
december24Solution1 = (\x -> search (hvacs x) (filterPermutations x) x) <$> input
  where
    hvacs = Map.keys . Map.filter isHVAC
    initialRobotPosition = hvacPosition 0
    filterPermutations x p = head p == initialRobotPosition x

december24Solution2 :: IO Int
december24Solution2 = (\x -> search (hvacs x) (filterPermutations x) x) <$> input
  where
    hvacs x = (Map.keys . Map.filter isHVAC) x ++ [initialRobotPosition x]
    initialRobotPosition = hvacPosition 0
    filterPermutations x p = head p == initialRobotPosition x && last p == initialRobotPosition x
