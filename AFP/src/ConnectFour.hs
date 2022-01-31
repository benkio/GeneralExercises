{-# LANGUAGE TypeApplications #-}

module ConnectFour where

import Data.Bifunctor (first)
import Data.List (foldl1)
import Data.List.NonEmpty (span, splitAt, tails, zip)
import qualified Data.List.NonEmpty as NL (filter, reverse, take)
import Data.Maybe (fromJust, isJust)
import Data.Tree
import Relude hiding (span, splitAt, tails, zip)
import Relude.Extra.Foldable1
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
      | otherwise = First $ Nothing

winCondition :: Board -> Player
winCondition b = (fromMaybe B . getFirst) $ columnsWinCondition <> rowsWinCondition <> leftDiagonalWinCondition <> rightDiagonalWinCondition
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
calculateGraph b = go b X depth
  where
    go b' p d = Node (b', p, winCondition b') $ if winCondition b' /= B || d <= 0 then [] else fmap (\x -> go x (nextPlayer p) (d -1)) (nextPossibleBoards b' p)

labelPropagation :: Board -> (Board, Player, Player) -> [(Board, Player)] -> (Board, Player)
labelPropagation _ (b, _, w) [] = (b, w)
labelPropagation rb (b, X, _) bs = first (\b' -> if b == rb then b' else b) $ maximumOn1 snd (fromList bs :: NonEmpty (Board, Player))
labelPropagation rb (b, O, _) bs = first (\b' -> if b == rb then b' else b) $ minimumOn1 snd (fromList bs :: NonEmpty (Board, Player))

minimax :: Tree (Board, Player, Player) -> Board
minimax t@(Node (b, _, _) _) = (fst . foldTree (labelPropagation b)) t

showGrid :: Board -> String
showGrid =
  ( foldr (\r acc -> acc ++ "\n" ++ ((foldl1 (++) . fmap show) r)) ""
      . transpose
      . fmap toList
      . toList
  )

-- Game IO

askMove :: Board -> IO Board
askMove b = do
  putStrLn $ showGrid b
  putStrLn "-----------"
  let moves = availableMoves b
  putStrLn $ "In which Column do you want to play?" ++ show moves
  columnString <- getLine
  let columnE = readEither @Int $ toString columnString
      retry = putStrLn "value not recognized or out of range, retry" >> askMove b
  either
    (const retry)
    ( \column ->
        if column `elem` moves
          then return $ applyMoveBoard b column X
          else retry
    )
    columnE

-- Utility Functions

prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList ls ne = case ls of
  [] -> ne
  (x : xs) -> x :| xs <> toList ne

appendList :: NonEmpty a -> [a] -> NonEmpty a
appendList (x :| xs) ys = x :| xs <> ys
