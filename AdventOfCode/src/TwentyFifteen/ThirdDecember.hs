module TwentyFifteen.ThirdDecember where

import Data.Bifunctor (bimap)
import Data.List (partition)
import Data.Set as S (Set, fromList, size, union)

data Direction
  = N
  | E
  | W
  | S
  deriving (Show)

type Coordinate = (Int, Int)

initialCoordinate :: Coordinate
initialCoordinate = (0, 0)

parseDirection :: String -> [Direction]
parseDirection [] = []
parseDirection ('^':xs) = N : parseDirection xs
parseDirection ('<':xs) = E : parseDirection xs
parseDirection ('>':xs) = W : parseDirection xs
parseDirection ('v':xs) = S : parseDirection xs
parseDirection (_:xs) = parseDirection xs

moveSanta :: Coordinate -> Direction -> Coordinate
moveSanta (x, y) N = (x, y + 1)
moveSanta (x, y) S = (x, y - 1)
moveSanta (x, y) E = (x + 1, y)
moveSanta (x, y) W = (x - 1, y)

visitedHouses :: [Direction] -> S.Set Coordinate
visitedHouses = S.fromList . scanl moveSanta initialCoordinate

input :: IO [Direction]
input = parseDirection <$> readFile "input/2015/3December.txt"

inputTest :: String
inputTest =
  ">\n\
            \^>v<\n\
            \^v^v^v^v^v"

solution1 :: [Direction] -> Int
solution1 = S.size . visitedHouses

solution1Test :: Bool
solution1Test =
  ((\l -> l == [2, 4, 2]) . fmap (solution1 . parseDirection) . lines) inputTest

splitDirections :: [Direction] -> ([Direction], [Direction])
splitDirections ds =
  (bimap (fmap snd) (fmap snd) . partition (\(x, _) -> x `mod` 2 == 0)) $
  [0 ..] `zip` ds

solution2Test :: Bool
solution2Test =
  ((\l -> l == [2, 3, 11]) . fmap (solution2 . parseDirection) . lines)
    inputTest

solution2 :: [Direction] -> Int
solution2 =
  S.size . uncurry S.union . bimap visitedHouses visitedHouses . splitDirections

thirdDecemberSolution1 :: IO Int
thirdDecemberSolution1 = solution1 <$> input

thirdDecemberSolution2 :: IO Int
thirdDecemberSolution2 = solution2 <$> input
