-------------------------------------------------------------------------------
--                           Advent Of Code - day 11                          --
-------------------------------------------------------------------------------
module TwentyTwenty.EleventhDecember where

import Data.Maybe (catMaybes)

data SeatStatus
  = Floor
  | Empty
  | Occupied
  deriving (Eq)

type Coordinate = (Int, Int)

type Seat = (Coordinate, SeatStatus)

type Grid = [Seat]

instance Show SeatStatus where
  show Empty = "L"
  show Floor = "."
  show Occupied = "#"

gridY :: Int
gridY = 92

gridX :: Int
gridX = 94

getSeat :: Grid -> Coordinate -> Seat --take advantage of the ordering
getSeat grid (x, y) = grid !! (y * (gridX + 1) + x)

showGrid :: Grid -> String
showGrid grid =
  fst $
    foldl
      ( \(acc, y) ((_, y'), s) ->
          if y /= y'
            then (acc ++ "\n" ++ show s, y')
            else (acc ++ show s, y)
      )
      ("", 0)
      grid

createSeat :: Char -> SeatStatus
createSeat 'L' = Empty
createSeat '#' = Occupied
createSeat '.' = Floor
createSeat _ = Floor

isOccupied :: Seat -> Bool
isOccupied (_, Occupied) = True
isOccupied _ = False

howManyOccupied :: Grid -> Int
howManyOccupied =
  foldl
    ( \acc s ->
        if isOccupied s
          then acc + 1
          else acc
    )
    0

buildGrid :: String -> Grid
buildGrid = (\l -> (>>=) l (uncurry buildRow)) . zip [0 ..] . lines
  where
    buildRow :: Int -> String -> Grid
    buildRow y r =
      (\(x, c) -> ((fromIntegral x, y), createSeat c)) <$> zip [0 ..] r

getNeighborsCoordinate :: Coordinate -> [Coordinate]
getNeighborsCoordinate (x, y) =
  [ (x', y')
    | x' <- [(max (x - 1) 0) .. (min (x + 1) gridX)],
      y' <- [(max (y - 1) 0) .. (min (y + 1) gridY)],
      x' /= x || y' /= y
  ]

getNeighbors :: Grid -> Coordinate -> Grid
getNeighbors grid coord = getSeat grid <$> getNeighborsCoordinate coord

expandCoordinateView :: Coordinate -> Coordinate -> Maybe Coordinate
expandCoordinateView (bx, by) (x, y) =
  validateCoord (x + oneOrZero (bx - x), y + oneOrZero (by - y))
  where
    validateCoord :: Coordinate -> Maybe Coordinate
    validateCoord (a, b) =
      if (a < 0 || a > gridX) || (b < 0 || b > gridY)
        then Nothing
        else Just (a, b)
    oneOrZero :: Int -> Int
    oneOrZero c
      | c < 0 = 1
      | c > 0 = -1
      | otherwise = 0

expandView :: Grid -> Coordinate -> Seat -> Maybe Seat
expandView grid c (c', Floor) =
  (getSeat grid <$> expandCoordinateView c c') >>= expandView grid c
expandView _ _ s = Just s

getNeighbors2 :: Grid -> Coordinate -> Grid
getNeighbors2 grid coord =
  catMaybes $ expandView grid coord <$> getNeighbors grid coord

computeSeatStatus :: Seat -> Grid -> Seat
computeSeatStatus seat@(c, st) neighbors
  | st == Empty && not (any isOccupied neighbors) = (c, Occupied)
  | st == Occupied && length (filter isOccupied neighbors) >= 4 = (c, Empty)
  | otherwise = seat

computeSeatStatus2 :: Seat -> Grid -> Seat
computeSeatStatus2 seat@(c, st) neighbors
  | st == Empty && not (any isOccupied neighbors) = (c, Occupied)
  | st == Occupied && length (filter isOccupied neighbors) >= 5 = (c, Empty)
  | otherwise = seat

computeNextStep :: Grid -> Grid
computeNextStep grid =
  fmap (\(c, s) -> computeSeatStatus (c, s) (getNeighbors grid c)) grid

computeNextStep2 :: Grid -> Grid
computeNextStep2 grid =
  fmap (\(c, s) -> computeSeatStatus2 (c, s) (getNeighbors2 grid c)) grid

computeMultipleSteps :: Int -> Grid -> Grid
computeMultipleSteps n grid = iterate computeNextStep grid !! n

computeUntilNoMoreChanges :: (Grid -> Grid) -> Grid -> Grid
computeUntilNoMoreChanges nextStep grid =
  let nextGrid = nextStep grid
   in if showGrid nextGrid == showGrid grid
        then nextGrid
        else computeUntilNoMoreChanges nextStep nextGrid

input :: IO Grid
input = buildGrid <$> readFile "input/2020/11December.txt"

eleventhDecemberSolution1 :: IO Int
eleventhDecemberSolution1 =
  howManyOccupied . computeUntilNoMoreChanges computeNextStep <$> input

eleventhDecemberSolution2 :: IO Int
eleventhDecemberSolution2 =
  howManyOccupied . computeUntilNoMoreChanges computeNextStep2 <$> input
