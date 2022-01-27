module ConnectFour where

import Data.Bifunctor (first)
import Data.List.NonEmpty (span, splitAt, tails, take, zip)
import qualified Data.List.NonEmpty as NL (filter)
import Data.Maybe (fromJust)
import Data.Tree
import Relude hiding (span, splitAt, tails, take, zip)
import Text.Printf (printf)

-- Parameters

rows :: Int
rows = 6

cols :: Int
cols = 7

win :: Int
win = 4

depth :: Int
depth = 6

-- Types

type Board = NonEmpty Column

type Column = NonEmpty Player

data Player = O | B | X
  deriving (Ord, Eq, Show)

-- Logic

initialBoard :: Board
initialBoard = fromList $ fromList . (\_ -> fmap (const B) [1 .. cols]) <$> [1 .. rows]

availableMoves :: Board -> [Int]
availableMoves b =
  ( fmap fst
      . NL.filter (\(_, c) -> any (== B) c)
      . zip (fromList [1 ..])
  )
    b

applyMoveColumn :: Column -> Player -> Column
applyMoveColumn c p
  | p == X = prependList (init blanks) (X :| postColumn)
  | p == O = prependList (init blanks) (O :| postColumn)
  | otherwise = error $ "Error while adding something to column"
  where
    (blanks, postColumn) = (first fromList . span (== B)) c

applyMoveBoard :: Board -> Int -> Player -> Board
applyMoveBoard b i p =
  let (prefix, postfix) = first fromList $ splitAt i b
      newCol = applyMoveColumn (last prefix) p
   in prependList (init prefix) (newCol :| postfix)

columnWin :: Column -> Bool
columnWin c = (any (\x -> all (== X) x || all (== O) x) . Relude.filter ((== win) . length)) $ transpose (take win (tails c))

calculateGraph :: Board -> Tree Board
calculateGraph = undefined

minimax :: Tree Board -> Board
minimax = undefined

-- Game IO

showGrid :: IO ()
showGrid = undefined

askMove :: IO Int
askMove = undefined

-- Utility Functions

prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList ls ne = case ls of
  [] -> ne
  (x : xs) -> x :| xs <> toList ne
