module ConnectFour where

import Data.Bifunctor (first)
import Data.List.NonEmpty (span, splitAt, tails, zip)
import qualified Data.List.NonEmpty as NL (filter, reverse, take)
import Data.Maybe (fromJust, isJust)
import Data.Tree
import Relude hiding (span, splitAt, tails, zip)
import Text.Printf (printf)
import qualified Text.Show

-- Parameters

rows :: Int
rows = 3

cols :: Int
cols = 3

win :: Int
win = 3

depth :: Int
depth = 6

-- Types

type Board = NonEmpty Column

type Column = NonEmpty Player

data Player = O | B | X
  deriving (Ord, Eq)

instance Show Player where
  show X = "X"
  show O = "O"
  show B = "."

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

columnWin :: Column -> First Player
columnWin c = (fold . fmap winner . Relude.filter ((== win) . length)) $ transpose (NL.take win (tails c))
  where
    winner :: [Player] -> First Player
    winner xs
      | all (== X) xs = First $ Just X
      | all (== O) xs = First $ Just O
      | otherwise = First $ Just B

winCondition :: Board -> Player
winCondition b = (fromJust . getFirst) $ columnsWinCondition <> rowsWinCondition <> leftDiagonalWinCondition <> rightDiagonalWinCondition
  where
    b' = toList <$> toList b
    findWinner = fold . fmap (columnWin . fromList)
    columnsWinCondition = findWinner b'
    rowsWinCondition = (findWinner . transpose) b'
    leftDiagonalWinCondition = (findWinner . transpose . zipWith (\i xs -> replicate i B ++ xs) [0 .. rows - 1]) b'
    rightDiagonalWinCondition = (findWinner . transpose . zipWith (\i xs -> replicate i B ++ xs) [0 .. rows - 1] . reverse) b'

nextPlayer :: Player -> Player
nextPlayer O = X
nextPlayer X = O
nextPlayer B = error "NextPlayer called with B"

-- Board, player to play, winner
calculateGraph :: Board -> Tree (Board, Player, Player)
calculateGraph b =
  unfoldTree
    ( \(x, p, _) ->
        ( (x, p, winCondition x),
          if winCondition x /= B then [] else fmap (\b -> (b, nextPlayer p, B)) (nextPossibleBoards x p)
        )
    )
    (b, X, B)

-- Better if there's a way to keep the tree structure so it's easier to unfold by label
getGraphDepth :: Tree (Board, Player, Player) -> [(Board, Player, Player)]
getGraphDepth = concat . take depth . levels

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
