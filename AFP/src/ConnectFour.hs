module ConnectFour where

import Data.Bifunctor (first)
import Data.List.NonEmpty (span, splitAt, tails, take, zip)
import qualified Data.List.NonEmpty as NL (filter, reverse)
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
  | p == X = appendList (NL.reverse (X :| preColumnReversed)) (init blanks)
  | p == O = appendList (NL.reverse (O :| preColumnReversed)) (init blanks)
  | otherwise = error $ "Error while adding something to column"
  where
    (blanks, preColumnReversed) = (first fromList . span (== B) . NL.reverse) c

applyMoveBoard :: Board -> Int -> Player -> Board
applyMoveBoard b i p =
  let (prefix, postfix) = first fromList $ splitAt i b
      newCol = applyMoveColumn (last prefix) p
   in prependList (init prefix) (newCol :| postfix)

nextPossibleBoards :: Board -> Player -> [Board]
nextPossibleBoards b p = (fmap (\i -> applyMoveBoard b i p) . availableMoves) b

columnWin :: Column -> Bool
columnWin c = (any (\x -> all (== X) x || all (== O) x) . Relude.filter ((== win) . length)) $ transpose (take win (tails c))

winCondition :: Board -> Bool
winCondition b = columnsWinCondition || rowsWinCondition || leftDiagonalWinCondition || rightDiagonalWinCondition
  where
    b' = toList b
    columnsWinCondition = any (columnWin) b'
    rowsWinCondition = (any (columnWin . fromList) . transpose . fmap toList) b'
    leftDiagonalWinCondition = (any (columnWin . fromList) . transpose . zipWith (\i xs -> replicate i B ++ xs) [0 .. rows - 1] . fmap toList) b'
    rightDiagonalWinCondition = (any (columnWin . fromList) . transpose . zipWith (\i xs -> replicate i B ++ xs) [0 .. rows - 1] . reverse . fmap toList) b'

nextPlayer :: Player -> Player
nextPlayer O = X
nextPlayer X = O
nextPlayer B = error "NextPlayer called with B"

calculateGraph :: Board -> Tree (Board, Player)
calculateGraph b =
  unfoldTree
    ( \(x, p) ->
        ( (x, p),
          fmap (\b -> (b, nextPlayer p)) (nextPossibleBoards x p)
        )
    )
    (b, X)

getGraphDepth :: Tree (Board, Player) -> [Board]
getGraphDepth = undefined --flatten . take depth . levels

minimax :: Tree Board -> Board
minimax = undefined

showGrid :: Board -> String
showGrid = (foldr (\r acc -> acc ++ "\n" ++ show r) "" . transpose . fmap toList . toList)

-- Game IO

askMove :: IO Int
askMove = undefined

-- Utility Functions

prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList ls ne = case ls of
  [] -> ne
  (x : xs) -> x :| xs <> toList ne

appendList :: NonEmpty a -> [a] -> NonEmpty a
appendList (x :| xs) ys = x :| xs <> ys
