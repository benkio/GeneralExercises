module TwentyFifteen.EighteenthDecember where

import Data.List
import Data.Map as Map
  ( filter
  , findMax
  , findWithDefault
  , fromList
  , mapWithKey
  , size
  , toList
  , updateAt
  , updateMax
  , updateMin
  )
import Data.Map (Map)

type Coordinate = (Int, Int)

type Grid = Map Coordinate Bool

input :: IO Grid
input = parseGrid <$> readFile "input/2015/18December.txt"

parseGrid :: String -> Grid
parseGrid =
  fromList .
  concatMap (\(y, l) -> (\(x, v) -> ((x, y), v == '#')) <$> zip [0 ..] l) .
  zip [0 ..] . lines

showGrid :: Grid -> String
showGrid m =
  (unlines .
   fmap
     (\l ->
        foldl
          (\acc' (_, c) ->
             acc' ++
             [ if c
                 then '#'
                 else '.'
             ])
          ""
          l) .
   groupBy (\((_, y), _) ((_, y'), _) -> y == y') .
   sortOn (\((x, y), _) -> x + y * ((+ 1) . snd . fst . Map.findMax) m) .
   Map.toList)
    m

inputTest :: Grid
inputTest =
  parseGrid
    ".#.#.#\n\
\...##.\n\
\#....#\n\
\..#...\n\
\#.#..#\n\
\####.."

neighboursCoordinates :: Coordinate -> [Coordinate]
neighboursCoordinates (x, y) =
  [(a, b) | a <- [x - 1 .. x + 1], b <- [y - 1 .. y + 1], a /= x || b /= y]

neighboursValues :: Grid -> Coordinate -> [Bool]
neighboursValues m c =
  (\x -> Map.findWithDefault False x m) <$> neighboursCoordinates c

computeNextStep :: Grid -> Grid
computeNextStep m = Map.mapWithKey (computeSingleCell m) m

computeSingleCell :: Grid -> Coordinate -> Bool -> Bool
computeSingleCell m c x
  | x && (onNeighbours == 2 || onNeighbours == 3) = True
  | not x && onNeighbours == 3 = True
  | otherwise = False
  where
    onNeighbours = (length . Prelude.filter id) $ neighboursValues m c

solution1 :: Int -> Grid -> Grid
solution1 steps m = iterate computeNextStep m !! steps

solution1Test :: Bool
solution1Test =
  let expectedGrid =
        parseGrid
          "......\n\
\......\n\
\..##..\n\
\..##..\n\
\......\n\
\......"
   in expectedGrid == solution1 4 inputTest

eighteenthDecemberSolution1 :: IO Int
eighteenthDecemberSolution1 = Map.size . Map.filter id . solution1 100 <$> input

solution2 :: Int -> Grid -> Grid
solution2 steps m =
  iterate (turnOnEdges . computeNextStep) (turnOnEdges m) !! steps

turnOnEdges :: Grid -> Grid
turnOnEdges m =
  let maxY = (snd . fst . Map.findMax) m
      m' =
        Map.updateMax (const (Just True)) $ Map.updateMin (const (Just True)) m
      m'' =
        Map.updateAt
          (\_ _ -> Just True)
          (Map.size m - 1 - maxY)
          (Map.updateAt (\_ _ -> Just True) maxY m')
   in m''

solution2Test :: Bool
solution2Test =
  let expectedGrid =
        parseGrid
          "##.###\n\
\.##..#\n\
\.##...\n\
\.##...\n\
\#.#...\n\
\##...#"
   in expectedGrid == solution2 5 inputTest

eighteenthDecemberSolution2 :: IO Int
eighteenthDecemberSolution2 = Map.size . Map.filter id . solution2 100 <$> input
