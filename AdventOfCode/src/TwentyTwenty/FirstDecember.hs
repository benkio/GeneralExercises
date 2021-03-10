-------------------------------------------------------------------------------
--                           Advent Of Code - day 1                          --
-------------------------------------------------------------------------------
module TwentyTwenty.FirstDecember
  ( firstDecemberSolution1,
    firstDecemberSolution2,
    finder,
  )
where

import Data.List (find)
import Data.Maybe (fromMaybe)

input :: IO [Int]
input = do
  content <- readFile "input/2020/1December.txt"
  (return . fmap (\n -> read n :: Int) . lines) content

solution :: Int -> [Int] -> Int
solution = finder 2020

finder :: Int -> Int -> [Int] -> Int
finder _ _ [] = 0
finder target 1 is = (fromMaybe 0 . find (target ==)) is
finder target n is =
  ( fromMaybe 0
      . find (0 /=)
      . fmap (\x -> x * finder (target - x) (n - 1) (dropWhile (x /=) is))
  )
    is

firstDecemberSolution1 :: IO Int
firstDecemberSolution1 = do
  solution 2 <$> input

firstDecemberSolution2 :: IO Int
firstDecemberSolution2 = do
  solution 3 <$> input
