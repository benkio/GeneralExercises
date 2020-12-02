module FirstDecember (firstDecemberSolution1, firstDecemberSolution2, finder) where

import           Data.List  (delete, find)
import           Data.Maybe (fromMaybe)

input :: IO [Int]
input = do
  content <- readFile "input/FirstDecember.txt"
  (return . fmap (\n -> read n :: Int ) . lines) content


solution1 :: [Int] -> Int
solution1 xs = finder xs 2020 2

finder :: [Int] -> Int -> Int -> Int
finder [] _ _      = 0
finder is target 1 = (fromMaybe 0 . find (target ==)) is
finder is target n = (fromMaybe 0 . find (0 /=) . fmap (\x -> x * finder (delete x is) (target-x) (n-1))) is

solution2 :: [Int] -> Int
solution2 xs = finder xs 2020 3

firstDecemberSolution1 :: IO Int
firstDecemberSolution1 = do solution1 <$> input

firstDecemberSolution2 :: IO Int
firstDecemberSolution2 = do solution2 <$> input
