-------------------------------------------------------------------------------
--                           Advent Of Code - day 5                          --
-------------------------------------------------------------------------------
module FifthDecember where

import Data.List (find, sort)
import Data.Maybe (fromJust)

data RowSelector
  = F
  | B
  deriving (Show)

data ColumnSelector
  = L
  | R
  deriving (Show)

class Selector a where
  isLowerHalf :: a -> Bool
  isRowSelector :: a -> Bool

instance Selector RowSelector where
  isLowerHalf F = True
  isLowerHalf B = False
  isRowSelector F = True
  isRowSelector B = True

instance Selector ColumnSelector where
  isLowerHalf L = True
  isLowerHalf R = False
  isRowSelector L = False
  isRowSelector R = False

data Seat =
  Seat
    { row :: Int
    , column :: Int
    , seatId :: Int
    }
  deriving (Show)

rowsRange :: (Int, Int)
rowsRange = (0, 127)

columnsRange :: (Int, Int)
columnsRange = (0, 7)

findRow :: (Selector s) => (Int, Int) -> s -> (Int, Int)
findRow (lr, hr) s =
  if isLowerHalf s
    then (lr, ((+ lr) . fromIntegral . floor . (/ 2) . realToFrac) (hr - lr))
    else (((+ lr) . fromIntegral . round . (/ 2) . realToFrac) (hr - lr), hr)

createRowSelector :: Char -> Maybe RowSelector
createRowSelector 'F' = Just F
createRowSelector 'B' = Just B
createRowSelector _ = Nothing

createColumnSelector :: Char -> Maybe ColumnSelector
createColumnSelector 'L' = Just L
createColumnSelector 'R' = Just R
createColumnSelector _ = Nothing

getSelectors :: String -> ([RowSelector], [ColumnSelector])
getSelectors =
  foldl
    (\(r, c) x ->
       maybe
         (maybe (r, c) (\y -> (r, c ++ [y])) (createColumnSelector x))
         (\y -> (r ++ [y], c))
         (createRowSelector x))
    ([], [])

calculateSeatId :: Int -> Int -> Int
calculateSeatId r c = r * 8 + c

calculateRow :: [RowSelector] -> Int
calculateRow xs =
  ((\x ->
      if isLowerHalf (last xs)
        then fst x
        else snd x) .
   foldl findRow rowsRange . init)
    xs

calculateColumn :: [ColumnSelector] -> Int
calculateColumn xs =
  ((\x ->
      if isLowerHalf (last xs)
        then fst x
        else snd x) .
   foldl findRow columnsRange . init)
    xs

selectSeat' :: ([RowSelector], [ColumnSelector]) -> Seat
selectSeat' (rs, cs) =
  let r = calculateRow rs
      c = calculateColumn cs
      sId = calculateSeatId r c
   in Seat {row = r, column = c, seatId = sId}

selectSeat :: [String] -> [Seat]
selectSeat xs = selectSeat' <$> fmap getSelectors xs

maxSeatId :: [Seat] -> Int
maxSeatId = foldr max 0 . fmap seatId

emptySeatId :: [Seat] -> Maybe Int
emptySeatId xs =
  let seatIdsOrdered = (sort . fmap seatId) xs
      minSeatId = head seatIdsOrdered
   in (fmap fst . find (uncurry (/=))) ([minSeatId ..] `zip` seatIdsOrdered)

input :: IO [String]
input = lines <$> readFile "input/5December.txt"

fifthDecemberSolution1 :: IO Int
fifthDecemberSolution1 = maxSeatId . selectSeat <$> input

fifthDecemberSolution2 :: IO Int
fifthDecemberSolution2 = fromJust . emptySeatId . selectSeat <$> input
