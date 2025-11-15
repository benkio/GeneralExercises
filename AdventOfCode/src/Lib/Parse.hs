module Lib.Parse (
    parseCommaSeparatedInts,
    parseGridWithElemSelection,
    parseNumGrid,
    parseTwoColumnNum,
    parseMove,
    parseCoords,
)
where

import Control.Applicative (many)
import Data.Bifunctor (bimap, first, second)
import Data.Either (either, isRight, partitionEithers, rights)
import Data.List.Split (wordsBy)
import Data.Maybe (mapMaybe)
import Data.Void (Void)
import Lib.Coord (Coord (..))
import Lib.Move (Move (..))
import Text.Megaparsec (Parsec, parse)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

{-
  3   4
  4   3
  2   5
  1   3
  3   9
  3   3
-}
parseTwoColumnNum :: String -> [(Int, Int)]
parseTwoColumnNum = fmap parseLine . lines
  where
    parseLine :: String -> (Int, Int)
    parseLine s = (parseNum s, (parseNum . dropWhile (== ' ') . dropWhile (/= ' ')) s)
    parseNum = (\x -> read x :: Int) . takeWhile (/= ' ')

{-
  7 6 4 2 1
  1 2 7 8 9
  9 7 6 2 1
  1 3 2 4 5
  8 6 4 4 1
  1 3 6 7 9
-}
parseNumGrid :: String -> [[Int]]
parseNumGrid = fmap parseInts . lines
  where
    parseInts :: String -> [Int]
    parseInts = fmap (\x -> read x :: Int) . words

{-
  ....#.....
  .........#
  ..........
  ..#.......
  .......#..
  ..........
  .#..^.....
  ........#.
  #.........
  ......#...
-}
parseGridWithElemSelection :: (Int -> Int -> Char -> Maybe (Either a b)) -> String -> ([a], [b])
parseGridWithElemSelection f =
    partitionEithers
        . concatMap
            ( \(y, s) ->
                mapMaybe
                    (uncurry (f y))
                    (zip [0 ..] s)
            )
        . zip [0 ..]
        . lines

{-
  75,47,61,53,29
  97,61,53,29,13
  75,29,13
  75,97,47,61,53
  61,13,29
  97,13,75,29,47
-}
parseCommaSeparatedInts :: String -> [Int]
parseCommaSeparatedInts = fmap ((\x -> read x :: Int) . take 2) . wordsBy (== ',')

{-
  <^^>>>vv<v>>v<<
-}
parseMove :: Char -> Move
parseMove '<' = L
parseMove '^' = U
parseMove '>' = R
parseMove 'v' = D

{-
5,4
4,2
4,5
3,0
-}
parseCoords :: String -> [Coord]
parseCoords = either (\e -> error ("[Parse]: error parsing choordinates " ++ show e)) id . parse (many parseCoord) ""
  where
    parseCoord :: Parsec Void String Coord
    parseCoord = do
        x <- decimal
        string ","
        y <- decimal
        newline
        return (x, y)
