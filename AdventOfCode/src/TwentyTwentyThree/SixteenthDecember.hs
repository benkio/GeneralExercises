{-# LANGUAGE TupleSections #-}

module TwentyTwentyThree.SixteenthDecember where

import Data.Map (Map, fromList)
import qualified Data.Map as M (lookup)
import Data.Set (Set)

type Cave = Map (Int, Int) Tile
data Direction = U | D | R | L
data Tile
    = E
    | VS
    | HS
    | MR
    | ML
    deriving (Eq)

instance Show Tile where
    show E = "."
    show VS = "|"
    show HS = "-"
    show MR = "/"
    show ML = "\\"

input :: IO Cave
input = parseInput <$> readFile "input/2023/16December.txt"

parseInput :: String -> Cave
parseInput =
    fromList
        . concatMap
            ( \(r, s) ->
                ( fmap (\(c, c') -> ((c, r), parseTile c'))
                    . zip [0 ..]
                )
                    s
            )
        . zip [0 ..]
        . lines

parseTile :: Char -> Tile
parseTile '.' = E
parseTile '|' = VS
parseTile '-' = HS
parseTile '/' = MR
parseTile '\\' = ML

testInput :: Cave
testInput =
    parseInput $ unlines [".|...\\....", "|.-.\\.....", ".....|-...", "........|.", "..........", ".........\\", "..../.\\\\..", ".-.-/..|..", ".|....-|.\\", "..//.|...."]

newDirections :: Direction -> Tile -> [Direction]
newDirections R VS = [U, D]
newDirections L VS = [U, D]
newDirections U HS = [R, L]
newDirections D HS = [R, L]
newDirections R MR = [U]
newDirections L MR = [D]
newDirections U MR = [R]
newDirections D MR = [L]
newDirections R ML = [D]
newDirections L ML = [U]
newDirections U ML = [L]
newDirections D ML = [R]
newDirections d _ = [d] -- E or pointy end of a splitter

newCoord :: (Int, Int) -> Direction -> (Int, Int)
newCoord (x, y) U = (x, y + 1)
newCoord (x, y) D = (x, y - 1)
newCoord (x, y) L = (x - 1, y)
newCoord (x, y) R = (x + 1, y)

newTiles :: Cave -> (Int, Int) -> Direction -> [((Int, Int), Direction)]
newTiles cs c d = (nextCord,) <$> nextDirs
  where
    nextCord = newCoord c d
    nextTile = M.lookup nextCord cs
    nextDirs = maybe [] (newDirections d) nextTile

-- lightBeamBounce :: Set (Int, Int) -> Cave -> (Int, Int) -> Direction -> (Set (Int, Int), (Int,Int), Direction)
-- lightBeamBounce vs cs c d = undefined
--   where nts = filter (`notMember` vs) $ newTiles cs c d

solution1 = undefined

sixteenthDecemberSolution1 :: IO Int
sixteenthDecemberSolution1 = undefined

solution2 = undefined

sixteenthDecemberSolution2 :: IO Int
sixteenthDecemberSolution2 = undefined
