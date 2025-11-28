{-# LANGUAGE TupleSections #-}

module TwentyTwentyThree.December16 where

import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.Bifunctor (bimap)
import Data.List (maximum, nubBy)
import Data.Map (Map, findMax, fromList)
import qualified Data.Map as M (lookup)
import Data.Set (Set, elems, empty, insert, notMember, size)
import Debug.Trace

type PositionDirection = ((Int, Int), Direction)

type Cave = Map (Int, Int) Tile

type Visited = Set PositionDirection

data Direction = U | D | R | L deriving (Show, Eq, Ord)

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
newCoord (x, y) U = (x, y - 1)
newCoord (x, y) D = (x, y + 1)
newCoord (x, y) L = (x - 1, y)
newCoord (x, y) R = (x + 1, y)

newTiles :: Cave -> (Int, Int) -> Direction -> [PositionDirection]
newTiles cs c d = (nextCord,) <$> nextDirs
  where
    nextCord = newCoord c d
    nextTile = M.lookup nextCord cs
    nextDirs = maybe [] (newDirections d) nextTile

lightBeamBounce :: Visited -> Cave -> [PositionDirection] -> Visited
lightBeamBounce vs _ [] = vs
lightBeamBounce vs cave ((c, d) : cs) = lightBeamBounce (insert (c, d) vs') cave (cs ++ nts)
  where
    nts = filter (`notMember` vs) $ newTiles cave c d
    vs' = foldr insert vs nts

solution1 :: PositionDirection -> Cave -> Int
solution1 p cs =
    -- traceShowId .
    (\x -> x - 1) . length . nubBy (\(a, _) (b, _) -> a == b) . elems $ lightBeamBounce empty cs [p]

december16Solution1 :: IO Int
december16Solution1 = solution1 ((-1, 0), R) <$> input

startingPoints :: Cave -> [PositionDirection]
startingPoints cave = top ++ left ++ right ++ down
  where
    ((mx, my), _) = findMax cave
    top = [((x, -1), D) | x <- [0 .. mx]]
    left = [((-1, y), R) | y <- [0 .. my]]
    down = [((x, my + 1), U) | x <- [0 .. mx]]
    right = [((mx + 1, y), L) | y <- [0 .. my]]

solution2 :: Cave -> Int
solution2 c = (maximum . parMap rdeepseq (`solution1` c) . startingPoints) c

december16Solution2 :: IO Int
december16Solution2 = solution2 <$> input
