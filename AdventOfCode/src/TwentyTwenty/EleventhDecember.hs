-------------------------------------------------------------------------------
--                           Advent Of Code - day 11                          --
-------------------------------------------------------------------------------
module TwentyTwenty.EleventhDecember where

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

gridRows :: Grid -> Int
gridRows = snd . fst . last

gridColumns :: Grid -> Int
gridColumns = fst . fst . last

getSeat :: Grid -> Coordinate -> Int -> Seat --take advantage of the ordering
getSeat grid (x, y) rows = grid !! (y * (rows + 1) + x)

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
howManyOccupied =
  foldl
    (\acc s ->
       if isOccupied s
         then acc + 1
         else acc)
    0

buildGrid :: String -> Grid
buildGrid = (\l -> (>>=) l (uncurry buildRow)) . zip [0 ..] . lines
  where
    buildRow :: Int -> String -> Grid
    buildRow y r =
      (\(x, c) -> ((fromIntegral x, y), createSeat c)) <$> zip [0 ..] r

getNeighborsCoordinate :: Coordinate -> (Int, Int) -> [Coordinate]
getNeighborsCoordinate (x, y) (rows, columns) =
  [ (x', y')
  | x' <- [(max (x - 1) 0) .. (min (x + 1) rows)]
  , y' <- [(max (y - 1) 0) .. (min (y + 1) columns)]
  , x' /= x || y' /= y
  ]

getNeighbors :: Grid -> Coordinate -> (Int, Int) -> Grid
getNeighbors grid coord gridSize =
  (\c -> getSeat grid c (fst gridSize)) <$>
  getNeighborsCoordinate coord gridSize

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

computeNextStep :: (Int, Int) -> Grid -> Grid
computeNextStep gridSize grid =
  fmap (\(c, s) -> computeSeatStatus (c, s) (getNeighbors grid c gridSize)) grid

computeMultipleSteps :: Int -> Grid -> (Int, Int) -> Grid
computeMultipleSteps n grid gridSize =
  iterate (computeNextStep gridSize) grid !! n

computeUntilNoMoreOccupied :: (Int, Int) -> Grid -> (Int, Grid)
computeUntilNoMoreOccupied gridSize grid =
  let computeList =
        ((\x -> x `zip` tail x) .
         fmap (\x -> (howManyOccupied x, x)) .
         iterate (computeNextStep gridSize))
          grid
   in (fst . head . snd) $ span (\((o, _), (o', _)) -> o /= o') computeList

input :: IO Grid
input = buildGrid <$> readFile "input/2020/11December.txt"

eleventhDecemberSolution1 :: IO Int
eleventhDecemberSolution1 =
  fst . (\x -> computeUntilNoMoreOccupied (gridRows x, gridColumns x) x) <$>
  input
