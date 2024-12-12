module Lib.Parse (
    parseCommaSeparatedInts,
    parseGridWithElemSelection,
    parseNumGrid,
    parseTwoColumnNum,
) where

import Data.Bifunctor (bimap, first, second)
import Data.Either (either, isRight, partitionEithers, rights)
import Data.List.Split (wordsBy)
import Data.Maybe (mapMaybe)

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
parseGridWithElemSelection :: (Int -> Int -> Char -> Maybe (Either a a)) -> String -> ([a], a)
parseGridWithElemSelection f s =
    second head
        . partitionEithers
        . concatMap
            ( \(y, s) ->
                mapMaybe
                    (uncurry (f y))
                    (zip [0 ..] s)
            )
        . zip [0 ..]
        . lines
        $ s

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
