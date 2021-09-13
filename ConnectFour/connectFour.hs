module ConnectFour where

import Data.Tree

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

type Board = [Row]
type Row = [Player]
data Player = O | B | X
  deriving (Ord, Eq, Show)

-- Logic

availableMoves :: Board -> [Int]
availableMoves = undefined

applyMove :: Board -> Int -> Board
applyMove = undefined

calculateGraph :: Board -> Tree Board
calculateGraph = undefined

minimax :: Tree Board -> Board
minimax = undefined

-- Game IO

showGrid :: IO ()
showGrid = undefined

askMove :: IO Int
askMove = undefined

