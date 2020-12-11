-------------------------------------------------------------------------------
--                           Advent Of Code - day 11                          --
-------------------------------------------------------------------------------
module TwentyTwenty.EleventhDecember where

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
createSeat _ = Floor

isOccupied :: Seat -> Bool
isOccupied (_, Occupied) = True
isOccupied _ = False

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

getNeighborsCoordinate :: Coordinate -> [Coordinate]
getNeighborsCoordinate (x, y) =
  [ (x', y')
  | x' <- [(max (x - 1) 0) .. (min (x + 1) gridX)]
  , y' <- [(max (y - 1) 0) .. (min (y + 1) gridY)]
  , x' /= x || y' /= y
  ]

getNeighbors :: Grid -> Coordinate -> Grid
getNeighbors grid coord =
  (\c -> getSeat grid c) <$> getNeighborsCoordinate coord

computeSeatStatus :: Seat -> Grid -> Seat
computeSeatStatus seat@(c, st) neighbors
  | st == Empty && not (any isOccupied neighbors) = (c, Occupied)
  | st == Occupied && length (filter isOccupied neighbors) >= 4 = (c, Empty)
  | otherwise = seat

computeNextStep :: Grid -> Grid
computeNextStep grid =
  fmap (\(c, s) -> computeSeatStatus (c, s) (getNeighbors grid c)) grid

computeMultipleSteps :: Int -> Grid -> Grid
computeMultipleSteps n grid = iterate computeNextStep grid !! n

computeUntilNoMoreChanges :: Grid -> Grid
computeUntilNoMoreChanges grid =
  let nextGrid = computeNextStep grid
   in if showGrid nextGrid == showGrid grid
        then nextGrid
        else computeUntilNoMoreChanges nextGrid

input :: IO Grid
input = buildGrid <$> readFile "input/2020/11December.txt"

eleventhDecemberSolution1 :: IO Int
eleventhDecemberSolution1 =
  howManyOccupied . (\x -> computeUntilNoMoreChanges x) <$> input
-- (\g -> ((\(x, y) -> showGrid x ++ "\n\n" ++ showGrid y) . head . snd . span (\(g, g') -> showGrid g /= showGrid g') . (\x -> x `zip` tail x) . iterate (computeNextStep (gridRows g, gridColumns g))) g) <$> E.input
