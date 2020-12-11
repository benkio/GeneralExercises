-------------------------------------------------------------------------------
--                           Advent Of Code - day 11                          --
-------------------------------------------------------------------------------
module TwentyTwenty.EleventhDecember where

import           Data.List  (find)
import           Data.Maybe (catMaybes)

data SeatStatus
  = Floor
  | Empty
  | Occupied

type Coordinate = (Int, Int)

type Seat = (Coordinate, SeatStatus)

type Grid = [Seat]

instance Show SeatStatus where
  show Empty    = "L"
  show Floor    = "."
  show Occupied = "#"

checkCoordinate :: Coordinate -> Coordinate -> Bool
checkCoordinate (x, y) (x', y') = x == x' && y == y'

getSeat :: Grid -> Coordinate -> Maybe Seat
getSeat grid coord' = find (checkCoordinate coord' . fst) grid

showGrid :: Grid -> String
showGrid grid =
  fst $
  foldl
    (\(acc, y) ((_, y'), s) ->
       if y /= y'
         then (acc ++ "\n" ++ show s, y')
         else (acc ++ show s, y))
    ("", 0)
    grid

createSeat :: Char -> SeatStatus
createSeat 'L' = Empty
createSeat '#' = Occupied
createSeat '.' = Floor
createSeat _   = Floor

isOccupied :: Seat -> Bool
isOccupied (_, Occupied) = True
isOccupied _             = False

howManyOccupied :: Grid -> Int
howManyOccupied = foldl (\acc s-> if isOccupied s then acc + 1 else acc) 0

buildGrid :: String -> Grid
buildGrid = (\l -> (>>=) l (uncurry buildRow)) . zip [0 ..] . lines
  where
    buildRow :: Int -> String -> Grid
    buildRow y r =
      (\(x, c) -> ((fromIntegral x, y), createSeat c)) <$> zip [0 ..] r

getNeighborsCoordinate :: Coordinate -> [Coordinate]
getNeighborsCoordinate (x, y) =
  [ (x', y')
  | x' <- [(max (x - 1) 0) .. x + 1]
  , y' <- [(max (y - 1) 0) .. y + 1]
  , x' /= x || y' /= y
  ]

getNeighbors :: Grid -> Coordinate -> Grid
getNeighbors grid coord =
  catMaybes $ getSeat grid <$> getNeighborsCoordinate coord

computeSeatStatus :: Seat -> Grid -> Seat
computeSeatStatus (c, Empty) neighbors =
  if (all (not . isOccupied) neighbors)
    then (c, Occupied)
    else (c, Empty)
computeSeatStatus (c, Occupied) neighbors =
  if length (filter isOccupied neighbors) >= 4
    then (c, Empty)
    else (c, Occupied)
computeSeatStatus (c, Floor) _ = (c, Floor)

computeNextStep :: Grid -> Grid
computeNextStep grid =
  fmap (\(c, s) -> computeSeatStatus (c, s) (getNeighbors grid c)) grid

computeMultipleSteps :: Int -> Grid -> Grid
computeMultipleSteps n grid = iterate computeNextStep grid !! n

computeUntilNoMoreOccupied :: Grid -> (Int, Grid)
computeUntilNoMoreOccupied grid =
  let computeList = ((\x -> x `zip` tail x) . fmap (\x -> (howManyOccupied x, x)) . iterate computeNextStep) grid
  in (fst . head . snd) $ span (\((o, _), (o', _)) -> o /= o') computeList

input :: IO Grid
input = buildGrid <$> readFile "input/2020/11December.txt"

eleventhDecemberSolution1 :: IO Int
eleventhDecemberSolution1 = fst . computeUntilNoMoreOccupied <$> input
