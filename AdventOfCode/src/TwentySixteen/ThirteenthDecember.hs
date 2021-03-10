module TwentySixteen.ThirteenthDecember where

import qualified Data.Set as Set

type Coordinate = (Int, Int)

input :: IO Int
input = (\x -> read x :: Int) <$> readFile "input/2016/13December.txt"

inputTest :: Int
inputTest = 10

isOpenSpace :: Int -> Coordinate -> Bool
isOpenSpace designerNum (x, y) =
  let value = x * x + 3 * x + 2 * x * y + y + y * y
      valueBinarySingle =
        until
          ((0 ==) . last)
          (\l -> init l ++ [snd (divMod (last l) 2), fst (divMod (last l) 2)])
          [value + designerNum]
   in (even . sum) valueBinarySingle

neighboors :: Int -> Coordinate -> [Coordinate]
neighboors designerNum (x, y) =
  [ (a, b)
    | a <- [(max 0 (x - 1)) .. x + 1],
      b <- [(max 0 (y - 1)) .. y + 1],
      (a == x || b == y) && (a /= x || b /= y),
      isOpenSpace designerNum (a, b)
  ]

search :: [Coordinate] -> Coordinate -> Int -> [Coordinate] -> Int -> Int
search is target step visited designerNum
  | target `elem` is = step
  | otherwise =
    let nextCoordinates =
          (filter (`notElem` visited) . concatMap (neighboors designerNum)) is
     in search nextCoordinates target (step + 1) (visited ++ is) designerNum

test :: Bool
test = search [(1, 1)] (7, 4) 0 [] 10 == 11

thirteenthDecemberSolution1 :: IO Int
thirteenthDecemberSolution1 = search [(1, 1)] (31, 39) 0 [] <$> input

fiftyIterationStatus ::
  [Coordinate] -> Int -> [Coordinate] -> Int -> [Coordinate]
fiftyIterationStatus is step visited designerNum
  | step == 50 = visited ++ is
  | otherwise =
    let nextCoordinates =
          (filter (`notElem` visited) . concatMap (neighboors designerNum)) is
     in fiftyIterationStatus
          nextCoordinates
          (step + 1)
          (visited ++ is)
          designerNum

thirteenthDecemberSolution2 :: IO Int
thirteenthDecemberSolution2 =
  length . Set.fromList . fiftyIterationStatus [(1, 1)] 0 [] <$> input
