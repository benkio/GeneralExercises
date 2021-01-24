module TwentyFifteen.TwentyfifthDecember where

import Data.Bifunctor
import Data.Map (Map)
import Data.Map as Map (findWithDefault, fromList)

type Coordinate = (Int, Int)

input :: IO Coordinate
input = parseRowColumn <$> readFile "input/2015/25December.txt"

parseRowColumn :: String -> Coordinate
parseRowColumn =
  (\(x, y) -> (read x :: Int, read y :: Int)) .
  (\s ->
     ( (init . reverse . takeWhile (' ' /=) . dropWhile (',' /=) . reverse) s
     , (init . reverse . takeWhile (' ' /=) . tail . reverse) s))

initialMap :: Map Coordinate Int
initialMap =
  (Map.fromList . concatMap parseRows . (`zip` [1 ..]) . lines)
    "20151125  18749137  17289845  30943339  10071777  33511524\n\
\31916031  21629792  16929656   7726640  15514188   4041754\n\
\16080970   8057251   1601130   7981243  11661866  16474243\n\
\24592653  32451966  21345942   9380097  10600672  31527494\n\
\   77061  17552253  28094349   6899651   9250759  31663883\n\
\33071741   6796745  25397450  24659492   1534922  27995004"

parseRows :: (String, Int) -> [(Coordinate, Int)]
parseRows (s, y) =
  fmap (\(v, x) -> ((x, y), read v :: Int)) (((`zip` [1 ..]) . words) s)

previousElementCoordinate :: Coordinate -> Coordinate
previousElementCoordinate (x, y)
  | x == 1 = (y - 1, x)
  | otherwise = (x - 1, y + 1)

solution1Test :: Bool
solution1Test = solution1 (Map.fromList [((1, 1), 20151125)]) (1, 2) == 31916031

solution1 :: Map Coordinate Int -> Coordinate -> Int
solution1 grid coord =
  let previousElem =
        Map.findWithDefault
          (solution1 grid (previousElementCoordinate coord))
          (previousElementCoordinate coord)
          grid
   in (previousElem * 252533) `mod` 33554393

solution2 = undefined

twentyfifthDecemberSolution1 :: IO Int
twentyfifthDecemberSolution1 =
  solution1 initialMap . (\(x, y) -> (y, x)) <$> input

twentyfifthDecemberSolution2 = undefined
