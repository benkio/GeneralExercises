{-# LANGUAGE OverloadedStrings #-}

module TwentyTwentyOne.SixthDecember where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Text as Text

parseInput :: String -> [Int]
parseInput = fmap (\x -> read (Text.unpack x) :: Int) . Text.splitOn "," . Text.pack

input :: IO [Int]
input = parseInput <$> readFile "input/2021/6December.txt"

inputTest :: [Int]
inputTest = parseInput "3,4,3,1,2"

emptyInput :: Map.Map Int Int
emptyInput = Map.fromList [(-1, 0), (0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0), (8, 0)]

groupByEta :: [Int] -> Map.Map Int Int
groupByEta = Map.unionWith (+) emptyInput . Map.fromList . fmap (\x -> (head x, length x)) . List.group . List.sort

evolve :: Map.Map Int Int -> Map.Map Int Int
evolve fs =
    let fs' = Map.foldrWithKey (\i v acc -> Map.insert (i - 1) v acc) emptyInput $ Map.filterWithKey (\k _ -> k /= -1) fs
        newBorn = fromJust $ Map.lookup (-1) fs'
        result = Map.filterWithKey (\k _ -> k /= -1) $ Map.insertWith (+) 8 newBorn $ Map.insertWith (+) 6 newBorn fs'
     in result

solution :: Int -> Map.Map Int Int -> Int
solution gg fs = (sum . Map.elems) $ iterate evolve fs !! gg

sixthDecemberSolution1 :: IO Int
sixthDecemberSolution1 = solution 80 . groupByEta <$> input

sixthDecemberSolution2 :: IO Int
sixthDecemberSolution2 = solution 256 . groupByEta <$> input
