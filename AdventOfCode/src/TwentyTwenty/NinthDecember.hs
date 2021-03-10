-------------------------------------------------------------------------------
--                           Advent Of Code - day 9                          --
-------------------------------------------------------------------------------
module TwentyTwenty.NinthDecember where

import Data.List (nub)

input :: IO [Int]
input =
  fmap (\x -> read x :: Int) . lines <$> readFile "input/2020/9December.txt"

addPairs :: [Int] -> [(Int, Int)]
addPairs xs = [(a, b) | a <- xs, b <- tail xs, a /= b]

isValid :: Int -> [(Int, Int)] -> Bool
isValid x = (x `elem`) . nub . fmap (uncurry (+))

-- Consider the list in a reverse order
findFirstInvalid :: [Int] -> [Int] -> Int
findFirstInvalid _ [] = 0
findFirstInvalid preamble (x : xs)
  | length preamble < 25 = findFirstInvalid (preamble ++ [x]) xs
  | length preamble == 25 && not (isValid x (addPairs preamble)) = x
  | otherwise = findFirstInvalid (tail preamble ++ [x]) xs

contiguousRangeSumToInvalid :: [Int] -> [Int] -> Int -> [Int]
contiguousRangeSumToInvalid [] _ _ = []
contiguousRangeSumToInvalid (x : xs) range invalid
  | sum range == invalid = range
  | sum range < invalid = contiguousRangeSumToInvalid xs (range ++ [x]) invalid
  | otherwise = contiguousRangeSumToInvalid (x : xs) (tail range) invalid

ninthDecemberSolution1 :: IO Int
ninthDecemberSolution1 = findFirstInvalid [] <$> input

ninthDecemberSolution2 :: IO Int
ninthDecemberSolution2 = do
  nums <- input
  let invalid = findFirstInvalid [] nums
      sumRange = contiguousRangeSumToInvalid nums [] invalid
  return $ minimum sumRange + maximum sumRange
