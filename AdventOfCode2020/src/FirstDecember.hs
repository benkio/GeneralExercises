module FirstDecember (firstDecemberSolution1, firstDecemberSolution2, finder) where

import Data.List (delete)
import Data.Maybe (listToMaybe, fromMaybe)

input :: IO [Int]
input = do
  content <- readFile "input/FirstDecember.txt"
  (return . fmap (\n -> read n :: Int ) . lines) content


solution1 :: [Int] -> Int
solution1 xs = finder xs 2020

finder :: [Int] -> Int -> Int
finder is target = (fromMaybe 0 . listToMaybe) [ x * y | x <- is, y <- is, x + y == target]

solution2 :: [Int] -> Int
solution2 xs = maximum $ fmap (\x -> x * finder (delete x xs) (2020-x)) xs

firstDecemberSolution1 :: IO Int
firstDecemberSolution1 = do solution1 <$> input

firstDecemberSolution2 :: IO Int
firstDecemberSolution2 = do solution2 <$> input
